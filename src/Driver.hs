module Main where

import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split (chunksOf)
import           Data.Maybe
import           System.Environment
import           System.Exit
import           System.IO
import           System.FilePath

import           Data.Graph.Inductive
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import           System.Console.Terminfo
                    (Terminal, Color(..), setupTermFromEnv,
                     getCapability, withForegroundColor, carriageReturn)

import           Kami
import           Parser
import           Progress

main :: IO ()
main =
  do fns <- getArgs
     when (null fns) (failure "No filename arguments provided")
     term <- setupTermFromEnv
     mapM_ (processPuzzle term) fns

openPuzzle :: PuzzleData -> (IntMap Coord, KamiGraph)
openPuzzle = renumber . buildGraph . removeBlanks . addCoordinates . puzTiles

getPaletteName :: FilePath -> String
getPaletteName = takeWhile isAlpha . takeBaseName

-- | Process a puzzle file by rendering it, computing a solution,
-- and rendering the solution.
processPuzzle :: Terminal -> FilePath -> IO ()
processPuzzle term fn =

  do putStrLn ""
     putStrLn ("Processing puzzle: " ++ fn)
     putStrLn ""

     puz <- load fn
     let pal = getPaletteName fn
         (loc, g) = openPuzzle puz

     putStrLn (prettyKami term pal puz)
     putStrLn ("Expected Moves: " ++ show (puzMoves puz))
     putStrLn ("Regions: " ++ show (noNodes g))
     putStrLn ("Connections: " ++ show (length (edges g)))

     (if noComponents g == 1 then normalMode else islandMode) term pal loc g


normalMode :: Terminal -> String -> IntMap Coord -> KamiGraph -> IO ()
normalMode term pal loc g =
  do mb <- printProgress term
         $ scaleProgress 100
         $ solve g

     case mb of
       Nothing -> failure "No solution"
       Just (sol,_) ->
         do putStrLn ("Solution: " ++ show (length sol))
            putStr (renderSolution term pal loc sol)


-- | Print an error message and terminate the program.
failure :: String {- ^ message -} -> IO a
failure err = hPutStrLn stderr err >> exitFailure


-- | Render a solution using the coordinates colored appropriately for
-- the move. Split up the moves 10 per line.
renderSolution ::
  Terminal          {- ^ configured terminal       -} ->
  String            {- ^ palette name              -} ->
  IntMap Coord      {- ^ map nodes to coordinates  -} ->
  [LNode TileColor] {- ^ nodes labeled with colors -} ->
  String
renderSolution term pal locs sol = unlines (intercalate ", " <$> chunksOf 10 steps)
  where
    steps = [ withColor term pal c (renderCoord (locs IntMap.! n)) | (n,c) <- sol]


-- | Wrap a content string with the control codes to give it
-- the requested color.
withColor ::
  Terminal  {- ^ configured terminal -} ->
  String    {- ^ palette name        -} ->
  TileColor {- ^ tile color          -} ->
  String    {- ^ content             -} ->
  String    {- ^ colored content     -}
withColor term pal (TileColor c) str =
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
   g1 = mkGraph (zip [1..] (map (TileColor . snd) xs))
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


renumber :: (IntMap a, KamiGraph) -> (IntMap a, KamiGraph)
renumber (m,g) =
  ( IntMap.fromList [ (f k, v) | (k,v) <- IntMap.toList m ]
  , mkGraph [ (f n, c) | (n,c) <- labNodes g ]
            [ (f x, f y, z) | (x,y,z) <- labEdges g] )
  where
    ns  = nodes g
    f n = fromJust (elemIndex n ns)


-- | Render rows of colors using the given palette as a triangular grid.
prettyKami :: Terminal -> String {- ^ palette -} -> PuzzleData -> String
prettyKami term pal puz =
  unlines $ [replicate h ' ' ++ columnLabels ] ++
            reverse (zipWith drawRow [0..] (puzTiles puz)) ++
            [columnLabels]
  where
    glyphs = "▲▼"

    drawRow i row =
      replicate i ' ' ++ intToDigit i : ' ' :
      concat (zipWith (\s c -> withColor term pal (TileColor c) [s])
                      (cycle glyphs)
                      row) ++ [' ', intToDigit i]

    h = length (puzTiles puz)
    w = maximum (map length (puzTiles puz)) `div` 2
    columnLabels = ' ' : concatMap showCol [0..w-1]
    showCol i | i < 10    = [' ', intToDigit i]
              | otherwise = ['₁', intToDigit (i-10)]

palette :: String {- ^ palette -} -> Int {- ^ color id -} -> Color
palette _    0 = Black
palette name n = cycle p !! max 0 (n - 1)
  where
    p = maybe fallback (map ColorNumber) (lookup name palettes)

    fallback = [Cyan, Red, Yellow, Blue, Magenta, Green, White]
    palettes =
      [("Bud"         ,[221,209,59 ,73 ,167,149])
      ,("Corners"     ,[80 ,168,59 ,82 ,221,231])
      ,("Hardline"    ,[114,59 ,203,203,124,221])
      ,("Hatch"       ,[229,95 ,72 ,209,82 ,82 ])
      ,("Hexy"        ,[47 ,59 ,122,47 ,215,167])
      ,("Islands"     ,[203,151,59 ,82 ,47 ,229])
      ,("Lines"       ,[95 ,221,60 ,224,73 ,83 ])
      ,("Nuc"         ,[203,59 ,47 ,83 ,187,83 ])
      ,("Reflect"     ,[72 ,230,59 ,47 ,47 ,203])
      ,("Rings"       ,[167,72 ,223            ])
      ,("Simplestripe",[203,188,47 ,59 ,74 ,59 ])
      ,("SJoin"       ,[59 ,231,47 ,47 ,215,131])
      ,("Squares"     ,[47 ,80 ,209,47 ,230,59 ])
      ,("Start"       ,[66 ,131,215,230        ])
      ,("Threed"      ,[83 ,203,95 ,79 ,82 ,83 ])
      ,("Threegrid"   ,[60 ,144,167,73 ,231,47 ])
      ,("TriAgain"    ,[59 ,79 ,47 ,47 ,59 ,203])
      ,("Tritarg"     ,[72 ,209,221,83 ,47 ,60 ])
      ,("Wall"        ,[47 ,59 ,168,83 ,79 ,215])]


-- | Print a period every 50 steps before returning the final value.
printProgress :: Terminal -> Progress a -> IO (Maybe (a, Progress a))
printProgress term start = putStr "░░░░░░░░░ 0" >> go 1 start
  where
    cr = fromMaybe "\n" (getCapability term carriageReturn)

    go _ Done           = Nothing <$ putStrLn ""
    go _ (Success x xs) = Just (x,xs) <$ putStrLn ""
    go i (Step x) =
      do let n = i `rem` 10
         putStr (cr ++ replicate n '▓' ++ replicate (9-n) '░' ++ ' ' : show i)
         hFlush stdout
         go (i+1) x

------------------------------------------------------------------------

islandMode ::
  Terminal     {- ^ terminal   -} ->
  String       {- ^ palette    -} ->
  IntMap Coord {- ^ locations  -} ->
  KamiGraph    {- ^ game graph -} ->
  IO ()
islandMode term pal loc g =
  let gs = [ nfilter (`elem` ns) g | ns <- components g, not (null (tail ns)) ]
  in for_ gs $ \g1 ->
       do putStrLn "*** Island ***"
          sols <- solveIsland term g1
          for_ sols $ \sol ->
            putStr ("* " ++ renderSolution term pal loc sol)

-- | Print out all of the solutions of the minimum length for this island
-- with up to one solution per color in the island.
solveIsland :: Terminal -> KamiGraph -> IO [[LNode TileColor]]
solveIsland term g =
  do mb <- printProgress term (solve g)
     case mb of
       Nothing          -> return []
       Just (sol, rest) ->
         next sol (colorsRemaining g)
                  (takeWhileProgress (\x -> length x == length sol) rest)

  where
    lastColor = snd . last

    next sol 1 _ = return [sol]
    next sol n prog =
      do mb <- printProgress term
             $ filterProgress (\x -> lastColor sol /= lastColor x) prog
         case mb of
           Nothing           -> return [sol]
           Just (sol1,prog') -> (sol:) <$> next sol1 (n-1) prog'
