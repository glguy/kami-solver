{-# Language DeriveFunctor, BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kami (KamiGraph, Color, solve, Progress(..), diameter) where

import           Control.Monad
import           Control.Monad.ST
import           Control.Parallel.Strategies
import           Data.Graph.Inductive
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable(..))
import qualified Data.IntSet as IntSet
import           Data.List
import           Data.Maybe
import           Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Vector.Unboxed.Mutable as U

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
  . astarOn summary searchCost (step goal)
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
  Int                  {- ^ solution length                -} ->
  SearchEntry          {- ^ current search state           -} ->
  [(SearchEntry, Int)] {- ^ (next search state, heuristic) -}
step limit (SearchEntry g path prev cost) =

  -- prune search states that will necessarily exceed the goal
  -- This happens outside of the generator to allow the
  -- heuristics to be computed in parallel.
  filter (\(e,h) -> searchCost e + h <= limit) $

  -- compute the heuristics for the next states in parallel
  withStrategy (parList rseq) $

  applyMove <$> if prev == 0 then initialMoveSet else forwardMoveSet prev

  where
    applyMove (n,c) = (entry, h)
      where
        g'         = changeColor n c g
        !h         = heuristic g'
        !entry     = SearchEntry g' ((n,c):path) n (cost + 1)

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
  (a -> b)         {- ^ state characterization -} ->
  (a -> Int)       {- ^ cost characterization  -} ->
  (a -> [(a,Int)]) {- ^ step function          -} ->
  a                {- ^ initial state          -} ->
  [a]              {- ^ reachable states       -}
astarOn rep cost nexts start = go HashSet.empty (P.singleton 0 start)
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
            work2 = foldl' addWork work1 (nexts x)

            addWork w (x',h) =
              P.insert (cost x' + h) x' w

------------------------------------------------------------------------

-- | Compute shortest path length between the two nodes that are furthest apart.
-- This implementation uses the Floyd-Warshal algorithm. In this implementation,
-- each edge is assumed to have weight 1, and the graph is assumed to be
-- undirected.
--
-- This implementation is only suitable for computing diameters up to 127.
-- This allows me to save a lot of space, and for the puzzles I'm solving
-- it's more than enough.
diameter :: Gr a b -> Int
diameter g | noNodes g <= 1 = 0
diameter g =
 let (lo,hi)  = nodeRange g
     !n       = hi - lo + 1
     !ns      = V.fromList (nodes g)
     toIx i j = (i-lo) * n + (j-lo)
 in runST $ do

     -- initialize to maxBound/2 so that two of them can be added without overflow
     a <- U.replicate (n*n) (maxBound `div` 2)
       :: ST s (U.MVector s Word8)

     -- set distances for each edge in graph to one
     forM_ (labEdges g) $ \(u,v,_) ->
        do U.unsafeWrite a (toIx u v) 1
           U.unsafeWrite a (toIx v u) 1

     -- set distance to self to zero
     V.forM_ ns $ \u -> U.unsafeWrite a (toIx u u) 0

     V.forM_ ns $ \k ->
       V.forM_ ns $ \i ->
         V.forM_ ns $ \j ->
           do ij <- U.unsafeRead a (toIx i j)
              ik <- U.unsafeRead a (toIx i k)
              kj <- U.unsafeRead a (toIx k j)
              when (ij > ik+kj) (U.unsafeWrite a (toIx i j) (ik + kj))

     fromIntegral . maximum <$> sequence
        [ U.unsafeRead a (toIx i j) | i:js <- tails (nodes g), j <- js]


------------------------------------------------------------------------

instance (V.Unbox a, Hashable a) => Hashable (V.Vector a) where
  hashWithSalt salt v = hashWithSalt salt (V.toList v)
