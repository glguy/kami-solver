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


progressFind :: (a -> Bool) -> [a] -> Progress (Maybe a)
progressFind _ [] = Done Nothing
progressFind p (x:xs)
  | p x       = Done (Just x)
  | otherwise = Step (progressFind p xs)

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
  { searchGraph :: KamiGraph     -- ^ current graph
  , searchPath  :: [LNode Color] -- ^ list of updates so far in reverse order
  , _searchLast :: !Node         -- ^ previous node updated or @0@
  , searchCost  :: !Int          -- ^ cached length of 'searchPath'
  }

initialEntry :: KamiGraph -> SearchEntry
initialEntry g = SearchEntry g [] 0 0


solve ::
  Int                            {- ^ target solution length -} ->
  KamiGraph                      {- ^ initial graph          -} ->
  Progress (Maybe [LNode Color]) {- ^ solution               -}
solve goal
  = (fmap . fmap) (reverse . searchPath)
  . progressFind (isSolved . searchGraph)
  . astarOn summary searchCost (heuristic . searchGraph) goal (step goal)
  . initialEntry

-- | Characterization of a particular search state used to eliminate unneeded
-- duplicate states.
summary :: SearchEntry -> V.Vector Int
summary (SearchEntry g _ p _) = V.fromList (p : concat [ [x,y] | (x,y) <- labNodes g ])

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
  Int           {- ^ solution length      -} ->
  SearchEntry   {- ^ current search state -} ->
  [SearchEntry] {- ^ next search state    -}
step limit (SearchEntry g path prev cost)
  | cost >= limit = []
  | otherwise     =

  applyMove <$> if prev == 0 then initialMoveSet else forwardMoveSet prev

  where
    applyMove (n,c) = SearchEntry g' ((n,c):path) n (cost + 1)
      where
        g' = changeColor n c g

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

-- | Implementation of A*.
--
-- The state characterization function is used to compute a value that will be
-- used to eliminate duplicate states from the search.
--
-- The step function should return all possible successor states of a given
-- state and an estimate of the cost of the remaining
-- steps needed to find the solution. This estimate must not exceed the actual
-- cost.
--
-- The resulting list of reachable states will be annotated with their cost and
-- will be in ascending order of cost.
astarOn ::
  Hashable b =>
  Ord b      =>
  (a -> b)   {- ^ state characterization -} ->
  (a -> Int) {- ^ cost characterization  -} ->
  (a -> Int) {- ^ heuristic              -} ->
  Int        {- ^ limit                  -} ->
  (a -> [a]) {- ^ step function          -} ->
  a          {- ^ initial state          -} ->
  [a]        {- ^ reachable states       -}
astarOn rep cost heur !limit nexts start = go HashSet.empty (P.singleton 0 start)
  where
    go seen work =
      case P.minView work of
        Nothing -> []
        Just (x,work1)
          | HashSet.member r seen -> go seen work1
          | otherwise             -> x : go seen' work2
          where
            r     = rep x
            seen' = HashSet.insert r seen
            work2 = foldl' addWork work1
                   $ parMap (evalTuple2 r0 rseq)
                       (\e -> (e, heur e))
                       (nexts x)

            addWork w (x',h)
              | prio > limit = w
              | otherwise    = P.insert (cost x' + h) x' w
              where prio = cost x' + h

------------------------------------------------------------------------

instance (V.Unbox a, Hashable a) => Hashable (V.Vector a) where
  hashWithSalt salt v = hashWithSalt salt (V.toList v)

------------------------------------------------------------------------

foreign import ccall "graph_diameter" c_graph_diameter ::
  Ptr Int -> Int -> Ptr Int -> Int -> IO Int

diameter :: KamiGraph -> Int
diameter g =
  let es = [x | (i,j) <- edges g, x <- [i,j]]
      ns = nodes g
  in
  unsafePerformIO $
  withArrayLen ns $ \nodesLen nodesPtr ->
  withArrayLen es $ \edgesLen edgesPtr ->
  c_graph_diameter nodesPtr nodesLen edgesPtr edgesLen
