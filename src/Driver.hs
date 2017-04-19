{-# Language OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Text (Text)
import           System.Environment
import           System.Exit
import           System.IO

import           Data.Graph.Inductive
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import           System.Console.Terminfo
                    (Terminal, Color(..), setupTermFromEnv,
                     getCapability, withForegroundColor)

import           Kami (KamiGraph, solve, Progress(..))
import           Parser

main :: IO ()
main =
  do fns <- getArgs
     when (null fns) (failure "No filename arguments provided")
     term <- setupTermFromEnv
     mapM_ (processPuzzle term) fns


-- | Process a puzzle file by rendering it, computing a solution,
-- and rendering the solution.
processPuzzle :: Terminal -> FilePath -> IO ()
processPuzzle term fn =

  do putStrLn ""
     putStrLn ("Processing puzzle: " ++ fn)
     putStrLn ""

     puz <- load fn
     let (locations, g) = buildGraph (addCoordinates (puzColors puz))

     putStrLn (prettyKami term puz)
     putStrLn ("Palette: " ++ show (puzPalette puz))
     putStrLn ("Expected Moves: " ++ show (puzMoves puz))
     putStrLn ("Regions: " ++ show (noNodes g))
     putStrLn ("Connections: " ++ show (length (edges g)))

     mb <- printProgress (solve (puzMoves puz) g)

     case mb of
       Nothing  -> failure "No solution"
       Just sol ->
         do putStrLn ("Solution: " ++ show (length sol))
            putStrLn (renderSolution term (puzPalette puz) locations sol)


-- | Print an error message and terminate the program.
failure :: String {- ^ message -} -> IO a
failure err = hPutStrLn stderr err >> exitFailure


renderSolution ::
  Terminal     {- ^ configured terminal       -} ->
  Text         {- ^ palette name              -} ->
  IntMap Coord {- ^ map nodes to coordinates  -} ->
  [LNode Int]  {- ^ nodes labeled with colors -} ->
  String
renderSolution term pal locs sol =
  intercalate ", "
    [ withColor term pal c (renderCoord (locs IntMap.! n)) | (n,c) <- sol]


-- | Wrap a content string with the control codes to give it
-- the requested color.
withColor ::
  Terminal {- ^ configured terminal -} ->
  Text     {- ^ palette name        -} ->
  Int      {- ^ color id            -} ->
  String   {- ^ content             -} ->
  String   {- ^ colored content     -}
withColor term pal c str =
  case getCapability term withForegroundColor of
    Just f  -> f (palette pal c) str
    Nothing -> str


-- | Transform a list of coordinate/color pairs into a graph
-- of color regions suitable for passing to the solver.
--
-- This function also returns a reverse mapping of nodes back
-- to coordinates to help translate the solution back to the
-- original coordinate system.
buildGraph :: [(Coord,Int)] -> (IntMap Coord, KamiGraph)
buildGraph xs = (locs, g2)
  where
   -- mapping of coordinates to node ids
   m = Map.fromList (zip (map fst xs) [1..])

   -- reverse mapping of nodes back to coordinates, one per region
   locs = IntMap.fromList
          [ (n,coord) | (coord, n) <- Map.toList m
                      , gelem n g2 ]

   -- graph with one node per triangle
   g1 :: KamiGraph
   g1 = mkGraph (zip [1..] (map snd xs))
          [ (m Map.! coord, m Map.! coord', ())
                 | (coord, _) <- xs
                 , coord' <- Parser.neighbors1 coord
                 , coord' `Map.member` m ]

   -- graph with one node per colored region
   g2 = foldl' aux g1 (nodes g1)

   -- collapse the region containing node @n@
   aux g n =
     case lab g n of
       Nothing -> g
       Just c  -> ([], n, c, (,)() <$> newNeighbors) & delNodes region g
         where
           region = udfs [n] (labfilter (==c) g)
           newNeighbors =
             nub (concatMap (Data.Graph.Inductive.neighbors g) region)
               \\ region


-- | Render rows of colors using the given palette as a triangular grid.
prettyKami :: Terminal -> PuzzleData -> String
prettyKami term puz =
  unlines $ [replicate h ' ' ++ columnLabels ] ++
            reverse (zipWith drawRow [0..] (puzColors puz)) ++
            [columnLabels]
  where
    glyphs = "▲▼"

    drawRow i row =
      replicate i ' ' ++ intToDigit i : ' ' :
      concat (zipWith (\s c -> withColor term (puzPalette puz) c [s])
                      (cycle glyphs)
                      row) ++ [' ', intToDigit i]

    h = length (puzColors puz)
    w = maximum (map length (puzColors puz)) `div` 2
    columnLabels = ' ' : concatMap showCol [0..w-1]
    showCol i | i < 10    = [' ', intToDigit i]
              | otherwise = ['₁', intToDigit (i-10)]


-- | Mapping of palette names and color numbers to terminal colors
palette :: Text {- ^ palette -} -> Int {- ^ color -} -> Color
palette _ 0 = Black

palette "Start" 1 = Cyan
palette "Start" 2 = Red
palette "Start" 3 = Yellow

palette "Squares" 6 = ColorNumber 0x13 -- navy
palette "Squares" 5 = White
palette "Squares" 2 = Cyan
palette "Squares" 3 = ColorNumber 0xd0 -- orange

palette "2Simplestripe" 6 = Blue
palette "2Simplestripe" 4 = Blue
palette "2Simplestripe" 2 = White
palette "2Simplestripe" 1 = Red

palette "Rings" 2 = Cyan
palette "Rings" 3 = White
palette "Rings" 1 = Red

palette "Wall" 2 = ColorNumber 0x34 -- brown
palette "Wall" 3 = ColorNumber 0x7d -- fucsia
palette "Wall" 5 = Cyan
palette "Wall" 6 = ColorNumber 0xd0 -- orange

palette "Tritarg" 1 = Cyan
palette "Tritarg" 2 = ColorNumber 0xd0 -- orange
palette "Tritarg" 3 = Yellow
palette "Tritarg" 6 = ColorNumber 0x13 -- navy

palette "TriAgain" 1 = ColorNumber 0x34 -- brown
palette "TriAgain" 2 = Cyan
palette "TriAgain" 6 = Red

palette "Hexy" 2 = ColorNumber 0x13 -- navy
palette "Hexy" 3 = Cyan
palette "Hexy" 5 = ColorNumber 0xd0 -- orange
palette "Hexy" 6 = Red

palette _ 1 = Cyan
palette _ 2 = Red
palette _ 3 = Yellow
palette _ 4 = Blue
palette _ 5 = Magenta
palette _ 6 = Green
palette _ 7 = Magenta

palette _ n = error ("Unknown color: " ++ show n)


-- | Print a period every 50 steps before returning the final value.
printProgress :: Progress a -> IO a
printProgress = go (0 :: Int)
  where
    go _  (Done x) = x <$ putStrLn ""
    go 50 x        = putChar '.' >> hFlush stdout >> go 0 x
    go i  (Step x) = go (i+1) x
