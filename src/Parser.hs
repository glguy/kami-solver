{-# Language OverloadedStrings #-}

module Parser
  ( -- * Coordinates
    Coord
  , renderCoord
  , neighbors1

    -- * Files
  , PuzzleData(..)
  , load
  , addCoordinates

    -- * Errors
  , ParseError(..)
  )  where

import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.List.Split
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text


-- | Representation of the some of the components of a puzzle
-- file.
data PuzzleData = PuzzleData
  { puzPalette :: Text    -- ^ name of the color palette
  , puzMoves   :: !Int    -- ^ moves required for "perfect" solution
  , puzColors  :: [[Int]] -- ^ rows of color data
  } deriving (Read, Show, Eq, Ord)


-- | Load a puzzle file and parse out the palette, move count,
-- and color information. Colors are grouped by rows.
-- Upon failed puzzle parsing, a 'ParseError' will be raised.
load ::
  FilePath      {- ^ puzzle filename        -} ->
  IO PuzzleData {- ^ palette, moves, colors -}
load fn =
  do res <- parse <$> Text.readFile fn
     case res of
       Right x -> return x
       Left  e -> throwIO e

------------------------------------------------------------------------

data ParseError
  = BadNumber !Text      -- ^ Unable to parse text as a number
  | BadFields !Int       -- ^ Unexpected number of fields in file
  | DecompressionFailure -- ^ Unable to decompress color data
  deriving (Eq, Ord, Show)

instance Exception ParseError

------------------------------------------------------------------------

-- | Puzzle coordinate with row, column, and side (0/1)
data Coord = C !Int !Int !Int deriving (Eq, Ord, Show)

-- | Render a coordinate to a compact format @row:column:side@.
renderCoord :: Coord -> String
renderCoord (C x y s) =
  (shows x . showChar ':' . shows y . showChar ':' . shows s) ""

-- | Transforms rows of color data into coordinate/color pairs.
addCoordinates ::
  [[Int]]          {- ^ row major layout       -} ->
  [ (Coord, Int) ] {- ^ coordinate/color pairs -}
addCoordinates xss =
  [ (C r c s, tri)
         | (r,row) <- zip [0..] xss
         , (c,squ) <- zip [0..] (chunksOf 2 row)
         , (s,tri) <- zip [0,1] squ
         , tri /= 0 ]

-- | single direction neighbors for building less redundant graph
neighbors1 :: Coord -> [Coord]
neighbors1 (C r c 0) = [ C r c 1 ]
neighbors1 (C r c _) = [ C r (c+1) 0, C (r+1) c 0 ]


parse ::
  Text {- ^ puzzle file content -} ->
  Either ParseError PuzzleData
parse str =
  case Text.splitOn "|" str of
    "01":pal:_lowX:_lowY:width:uncLen:comDat:moves:_ ->

      do w <- parseNumber width
         l <- parseNumber uncLen
         m <- parseNumber moves
         let dat = uncompress (Text.unpack comDat)

         unless (length dat == l) (Left DecompressionFailure)

         let colors = chunksOf (2*w) (map digitToInt dat)

         return PuzzleData { puzPalette = pal
                           , puzMoves   = m
                           , puzColors  = colors }

    xs -> Left $! BadFields (length xs)

parseNumber :: Text -> Either ParseError Int
parseNumber str =
  case Text.signed Text.decimal str of
    Right (n,"") -> Right n
    _            -> Left (BadNumber str)

-- | Expand a compressed (run-level encoded) puzzle string.
uncompress :: String -> String
uncompress (c:'X':x1:x2:xs) = replicate (read [x1,x2]) c ++ uncompress xs
uncompress (c:m:xs) | isAsciiLower m =
  replicate (ord m - ord 'a' + 3) c ++ uncompress xs
uncompress (c:xs) = c : uncompress xs
uncompress [] = []
