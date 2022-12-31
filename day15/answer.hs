{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

import AOC.IO
import AOC.Utils
import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import Data.Maybe
import Debug.Trace
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Sequence as SQ
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

-- part to show
part = 2

-- will submit?
willSubmit = 0

type Range = (Int, Int)
type Sensor = (Coord, Coord)
type SensorRange = (Coord, Int)
type Line = (Int, Int)

parseSensor :: String -> Sensor
parseSensor = mapTuple2 (mapTuple2 read) . (\xs -> ((xs!!1, xs!!3), (xs!!5, xs!!7))) . splitOneOf "=,:"

sensorRange :: Sensor -> SensorRange
sensorRange (s, b) = (s, manhattanDist s b)

manhattanDist :: Coord -> Coord -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getRange :: Int -> SensorRange -> Range
getRange y ((a, b), r) = (a - (r - abs (y - b)), a + (r - abs (y - b)))

mergeRange :: [Range] -> [Range] -> [Range]
mergeRange [] (s:src) = mergeRange [s] src
mergeRange des [] = reverse des
mergeRange fdes@((ds, de):des) (s@(ss, se):src) = mergeRange ndes src
  where ndes
          | ss > se = fdes
          | ss <= de = (ds, max de se):des
          | otherwise = s:fdes

solveP1' :: Int -> [Sensor] -> Int
solveP1' y s = (sum $ map (uncurry (flip (-))) ranges) - beacons + 1
  where ranges = mergeRange [] $ sort $ map (getRange y . sensorRange) s
        beacons = length $ nub $ filter ((==) y . snd) $ map snd s

makeLines :: SensorRange -> [Line]
makeLines = concatMap (\((x, y), x') -> [(1, y - x), (-1, y - x + (2 * x'))]) . (\((x, y), r) -> [((x + r + 1, y), x), ((x - r - 1, y), x)])

tuningFreq :: Coord -> Int
tuningFreq (x, y) = x * 4000000 + y

solveP2':: Range -> [SensorRange] -> Coord
solveP2' lm sr = head $ filterCoords $ concatMap (map getIntersectCoord . uncurry pairs2) $ pairs $ map makeLines sr
  where filterCoords = filter (\a -> all (\(b, r) -> (manhattanDist a b) > r) sr) . filter (uncurry ((&&) `on` uncurry between lm))
        getIntersectCoord ((a1, b1), (a2, b2)) = (x, y)
          where x = (b2 - b1) `div` 2
                y = a1 * x + b1

solveP1 = solveP1' 2000000 . map parseSensor . lines
solveP2 = tuningFreq . solveP2' (0, 4000000) . map (sensorRange . parseSensor) . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

