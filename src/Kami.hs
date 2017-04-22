{-# Language ForeignFunctionInterface, DeriveFunctor, BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kami (KamiGraph, Color, Progress(..), solve) where

import           Control.Parallel.Strategies
import           Data.Graph.Inductive
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable(..))
import qualified Data.IntSet as IntSet
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.PQueue.Prio.Min as P

import           Foreign (Ptr, withArrayLen)
import           System.IO.Unsafe (unsafePerformIO)

type Color = Int
type KamiGraph = Gr Color ()

data Progress a = Step (Progress a) | Done a deriving Functor

------------------------------------------------------------------------

isSolved :: KamiGraph -> Bool
isSolved g = 1 == noNodes g

-- | Assign a new color to a node and merge all connected nodes matching
-- the new color
changeColor :: Node -> Color -> KamiGraph -> KamiGraph
changeColor n c g = ([], n, c, (,)() <$> newNeighbors) & delNodes (n:mergeNodes) g
  where
    (mergeNodes, keepNodes) = partition (\m -> Just c == lab g m) (neighbors g n)

    newNeighbors
      = nub (keepNodes ++ [ x | m <- mergeNodes, x <- neighbors g m ])
      \\ (n:mergeNodes)

data SearchEntry = SearchEntry
  { searchPath  :: [LNode Color] -- ^ list of updates so far in reverse order
  , searchCost  :: !Int          -- ^ cached length of 'searchPath'
  }

searchGraph :: KamiGraph -> SearchEntry -> KamiGraph
searchGraph g e =
  foldl' (\acc (n,c) -> changeColor n c acc) g (reverse (searchPath e))

initialEntry :: SearchEntry
initialEntry = SearchEntry [] 0


solve ::
  Int                            {- ^ target solution length -} ->
  KamiGraph                      {- ^ initial graph          -} ->
  Progress (Maybe [LNode Color]) {- ^ solution               -}
solve goal g
  = (fmap . fmap) (reverse . searchPath)
  $ astar goal g

-- | Characterization of a particular search state used to eliminate unneeded
-- duplicate states.
summary :: KamiGraph -> V.Vector Int
summary g = V.fromList (concat [ [x,y] | (x,y) <- labNodes g ])

-- | Compute the number of unique colors remaining in the graph.
colorsRemaining :: KamiGraph -> Int
colorsRemaining = IntSet.size . IntSet.fromList . map snd . labNodes


-- | Compute the unique list of the colors of nodes that are neighbors
-- to the given node.
adjacentColors :: KamiGraph -> Node -> [Color]
adjacentColors g m = nub [ c | Just c <- lab g <$> neighbors g m ]


-- | Compute the next possible search states from a given search state along
-- with the cost of the step and the lower-bound estimate of the number of
-- moves remaining until a solution.
step ::
  KamiGraph ->
  Int                 {- ^ solution length                       -} ->
  SearchEntry         {- ^ current search state                  -} ->
  [(SearchEntry,Int)] {- ^ next search state and heuristic value -}
step g limit (SearchEntry path cost)
  | cost >= limit = []
  | otherwise     =

  withStrategy (parList (evalTuple2 r0 rseq)) $
  applyMove <$> case path of
                  []         -> initialMoveSet
                  (prev,_):_ -> forwardMoveSet prev

  where
    applyMove (n,c) = (SearchEntry ((n,c):path) (cost + 1), h)
      where
        g' = changeColor n c g
        h  = heuristic g'

    initialMoveSet =
      [ (n,c) | n <- nodes g, c <- adjacentColors g n ]

    forwardMoveSet n =
      [ (n,c) | c <- adjacentColors g n ] ++
      [ (o, fromJust (lab g n)) | o <- neighbors g n ]


-- | Compute a lower-bound on the number of moves that a graph can be
-- solved in.
heuristic :: KamiGraph -> Int
heuristic g = max ((diameter g + 1) `div` 2)
                  (colorsRemaining g - 1)

------------------------------------------------------------------------

-- | Implementation of A* search to find a solution to a KAMI puzzle.
--
-- A* prioritizes path possibilities by choosing those that have the
-- minimum estimate of the lower-bound on the solution length starting
-- from a particular search state.
astar :: Int -> KamiGraph -> Progress (Maybe SearchEntry)
astar !limit start = go HashSet.empty (P.singleton 0 initialEntry)
  where
    go seen work =
      case P.minView work of
        Nothing                   -> Done Nothing
        Just (x,work1)
          | isSolved g            -> Done (Just x)
          | HashSet.member r seen -> go seen work1
          | otherwise             -> Step (go seen' work2)
          where
            r     = summary g
            g     = searchGraph start x
            seen' = HashSet.insert r seen
            work2 = foldl' addWork work1 (step g limit x)

            addWork w (x',h)
              | prio > limit = w
              | otherwise    = P.insert prio x' w
              where prio = searchCost x' + h

------------------------------------------------------------------------

instance (V.Unbox a, Hashable a) => Hashable (V.Vector a) where
  hashWithSalt salt v = hashWithSalt salt (V.toList v)

------------------------------------------------------------------------

foreign import ccall "graph_diameter" c_graph_diameter ::
  Ptr Int -> Int -> Ptr Int -> Int -> IO Int

diameter :: KamiGraph -> Int
diameter g =
  unsafePerformIO $
  withArrayLen ns $ \nodesLen nodesPtr ->
  withArrayLen es $ \edgesLen edgesPtr ->
  c_graph_diameter nodesPtr nodesLen edgesPtr edgesLen

  where
    es = [x | (i,j) <- edges g, x <- [i,j]] -- flat list of edges
    ns = nodes g                            -- ordered list of node ids
