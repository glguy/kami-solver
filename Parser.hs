{-# Language OverloadedStrings #-}

module Parser (load, Coord, addCoordinates, neighbors1)  where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Text.IO as Text
import Data.Char
import Data.List.Split
import Control.Monad


load :: FilePath -> IO (Text, Int, [[Int]])
load fn =
  do res <- parse <$> Text.readFile fn
     case res of
       Right x -> return x
       Left  e -> fail e


type Coord = (Int,Int,Int) -- row, col, side

addCoordinates ::
  [[Int]]          {- ^ row major layout       -} ->
  [ (Coord, Int) ] {- ^ coordinate/color pairs -}
addCoordinates xss =
  [ ((r,c,s), tri)
         | (r,row) <- zip [0..] xss
         , (c,squ) <- zip [0..] (chunksOf 2 row)
         , (s,tri) <- zip [0,1] squ
         , tri /= 0 ]

-- | single direction neighbors for building less redundant graph
neighbors1 :: Coord -> [Coord]
neighbors1 (r,c,0) = [ (r,c,1) ]
neighbors1 (r,c,_) = [ (r,c+1,0), (r+1,c,0) ]

parse :: Text -> Either String (Text, Int, [[Int]])
parse str =
  case Text.splitOn "|" str of
    "01":pal:_lowX:_lowY:width:uncLen:comDat:moves:_ ->

      do w <- parseNumber width
         l <- parseNumber uncLen
         m <- parseNumber moves
         let dat = uncompress (Text.unpack comDat)

         unless (length dat == l) (Left "failure in decompression")

         let grid = chunksOf (2*w) (map digitToInt dat)

         return (pal,m,grid)

    _ -> Left "Failed initial split"

parseNumber :: Text -> Either String Int
parseNumber str =
  do (n,rest) <- Text.signed Text.decimal str
     if Text.null rest then return n
                       else Left "trailing characters"

uncompress :: String -> String
uncompress (c:'X':x1:x2:xs) = replicate (read [x1,x2]) c ++ uncompress xs
uncompress (c:m:xs)
  | isAsciiLower m = replicate (ord m - ord 'a' + 3) c ++
                     uncompress xs
uncompress (c:xs) = c : uncompress xs
uncompress [] = []


