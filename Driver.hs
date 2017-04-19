{-# Language OverloadedStrings #-}

module Main where

import Kami (KamiGraph, solve, Progress(..))
import Parser
import System.Environment
import qualified Data.Map as Map
import Data.Graph.Inductive
import Data.Foldable
import System.Exit
import Data.List
import Data.Char
import System.IO
import Data.Text (Text)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import System.Console.Terminfo
          (Terminal, Color(..), setupTermFromEnv, getCapability, withForegroundColor)

main :: IO ()
main =
  do [fn]   <- getArgs
     (paletteName,m,xs) <- load fn
     let (locations, g) = buildGraph (addCoordinates xs)
     term <- setupTermFromEnv

     putStr (prettyKami term paletteName xs)
     putStrLn ("Palette: " ++ show paletteName)
     putStrLn ("Expected Moves: " ++ show m)
     putStrLn ("Regions: " ++ show (noNodes g))
     putStrLn ("Connections: " ++ show (length (edges g)))

     mb <- printProgress (solve m g)

     case mb of
       Nothing  -> failure "No solution"
       Just sol ->
         do putStrLn ("Solution: " ++ show (length sol))
            putStrLn (renderSolution term paletteName locations sol)

-- | Print an error message and terminate the program.
failure :: String -> IO a
failure err = hPutStrLn stderr err >> exitFailure


renderSolution :: Terminal -> Text -> IntMap Coord -> [LNode Int] -> String
renderSolution term pal locs sol =
  intercalate ", " [ withColor term pal c (showCoord (locs IntMap.! n)) | (n,c) <- sol]
  where
    showCoord (x,y,s) = shows x $ showChar ':' $ shows y $ showChar ':' $ show s

withColor :: Terminal -> Text -> Int -> String -> String
withColor term pal c str =
  case getCapability term withForegroundColor of
    Just f  -> f (palette pal c) str
    Nothing -> str

buildGraph :: [(Coord,Int)] -> (IntMap Coord, KamiGraph)
buildGraph xs = (locs, g2)
  where
   m = Map.fromList (zip (map fst xs) [1..])

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

   -- flood fill each node with its own color
   g2 = foldl' aux g1 (nodes g1)

   aux g n =
     case lab g n of
       Nothing -> g
       Just c  -> ([], n, c, (,)() <$> newNeighbors) &
                  delNodes region g
         where
            region = udfs [n] (labfilter (==c) g)
            newNeighbors = nub (concatMap (Data.Graph.Inductive.neighbors g) region)
                        \\ region


-- | Render rows of colors using the given palette as a triangular grid.
prettyKami :: Terminal -> Text {- ^ palette -} -> [[Int]] {- ^ colors -} -> String
prettyKami term pal g =
  unlines $ reverse (zipWith drawRow [0..] g) ++
            [columnLabels]
  where
    w = maximum (map length g) `div` 2
    glyphs = "▲▼"

    drawRow i row =
      replicate i ' ' ++ intToDigit i : ' ' :
      concat (zipWith (\s c -> withColor term pal c [s]) (cycle glyphs) row)

    columnLabels = ' ' : concatMap showCol [0..w-1]
    showCol i | i < 10    = [' ', intToDigit i]
              | otherwise = ['₁', intToDigit (i-10)]


palette :: Text -> Int -> Color
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
