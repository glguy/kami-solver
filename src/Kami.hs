{-# Language ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kami
  (KamiGraph, TileColor(..), Progress(..),
   solve, filterProgress, colorsRemaining) where

import           Control.Parallel.Strategies
import           Data.Graph.Inductive
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable(..))
import qualified Data.IntSet as IntSet
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.PQueue.Prio.Min as P

import           Progress

-- FFI related imports
import           Foreign (Ptr, withArrayLen)
import           System.IO.Unsafe (unsafePerformIO)

newtype TileColor = TileColor { tileColorId :: Int } deriving (Read, Show, Eq, Ord)
type KamiGraph = Gr TileColor ()

------------------------------------------------------------------------

isSolved :: KamiGraph -> Bool
isSolved g = 1 == noNodes g

-- | Assign a new color to a node and merge all connected nodes matching
-- the new color
changeColor :: Node -> TileColor -> KamiGraph -> KamiGraph
changeColor n c g = ([], n, c, (,)() <$> newNeighbors) & delNodes (n:mergeNodes) g
  where
    (mergeNodes, keepNodes) = partition (\m -> Just c == lab g m) (neighbors g n)

    newNeighbors
      = nub (keepNodes ++ [ x | m <- mergeNodes, x <- neighbors g m ])
      \\ (n:mergeNodes)

data SearchEntry = SearchEntry
  { searchPath  :: [LNode TileColor] -- ^ list of updates so far in reverse order
  , searchCost  :: !Int          -- ^ cached length of 'searchPath'
  } deriving (Read, Show)

initialEntry :: SearchEntry
initialEntry = SearchEntry [] 0


-- | Rebuild the game graph given the starting arrangement and a list of moves.
-- Rebuilding this graph allows the work queue to only contain the move list
-- which results in much less memory consumption. Rebuilding the graph is a
-- small portion of the computation cost of searching for a solution.
rebuildGraph :: KamiGraph -> SearchEntry -> KamiGraph
rebuildGraph g e = foldr (uncurry changeColor) g (searchPath e)


solve ::
  KamiGraph                  {- ^ initial graph -} ->
  Progress [LNode TileColor] {- ^ solution      -}
solve = fmap (reverse . searchPath) . astar

-- | Characterization of a particular search state used to eliminate unneeded
-- duplicate states.
summary :: KamiGraph -> V.Vector Int
summary g = V.fromList [ z | (x, TileColor y) <- labNodes g, z <- [x,y] ]

-- | Compute the number of unique colors remaining in the graph.
colorsRemaining :: KamiGraph -> Int
colorsRemaining = IntSet.size . IntSet.fromList . map (tileColorId . snd) . labNodes


-- | Compute the unique list of the colors of nodes that are neighbors
-- to the given node.
adjacentColors :: KamiGraph -> Node -> [TileColor]
adjacentColors g m = nub [ c | Just c <- lab g <$> neighbors g m ]


-- | Compute the next possible search states from a given search state along
-- with the cost of the step and the lower-bound estimate of the number of
-- moves remaining until a solution.
step ::
  KamiGraph ->
  SearchEntry         {- ^ current search state                  -} ->
  [(SearchEntry,Int)] {- ^ next search state and heuristic value -}
step g (SearchEntry path cost) =

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
astar :: KamiGraph -> Progress SearchEntry
astar start = go HashSet.empty (P.singleton minBound initialEntry)
  where
    go seen work =
      case P.minView work of
        Nothing                   -> Done
        Just (x,work1)
          | isSolved g            -> Success x (go seen' work1)
          | HashSet.member r seen -> go seen work1
          | otherwise             -> Step (go seen' work2)
          where
            r     = summary g
            g     = rebuildGraph start x
            seen' = HashSet.insert r seen
            work2 = foldl' addWork work1 (step g x)

            addWork w (x',h) = P.insert (Priority prio c) x' w
              where prio = c + h
                    c    = searchCost x'

------------------------------------------------------------------------

-- | This 'Priority' type considers elements small if their first component
-- is small, and in the case of a tie prefers the element with the larger
-- second component. This type will be used to prioritize the search queue
-- for minimizing the lower-bound on the solution length but maximizing
-- the depth searched.
data Priority = Priority !Int !Int -- heuristic cost
  deriving (Read, Show, Eq)

instance Bounded Priority where
  minBound = Priority minBound maxBound
  maxBound = Priority maxBound minBound

-- | Normal order on first component, reversed order on second component
instance Ord Priority where
  compare (Priority h1 c1) (Priority h2 c2) =
    compare h1 h2 `mappend` compare c2 c1

------------------------------------------------------------------------

instance (V.Unbox a, Hashable a) => Hashable (V.Vector a) where
  hashWithSalt salt v = hashWithSalt salt (V.toList v)

------------------------------------------------------------------------

foreign import ccall "graph_diameter" c_graph_diameter ::
  Ptr Int -> Int -> Ptr Int -> Int -> IO Int

-- | Compute graph diameter using Floyd-Warshal algorithm.
diameter :: KamiGraph -> Int
diameter g =
  unsafePerformIO $
  withArrayLen ns $ \nodesLen nodesPtr ->
  withArrayLen es $ \edgesLen edgesPtr ->
  c_graph_diameter nodesPtr nodesLen edgesPtr edgesLen

  where
    es = [x | (i,j) <- edges g, x <- [i,j]] -- flat list of edges
    ns = nodes g                            -- ordered list of node ids
