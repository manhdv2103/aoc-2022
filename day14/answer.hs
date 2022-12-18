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
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Sequence as SQ

-- part to show
part = 2

-- will submit?
willSubmit = 0

type Coord = (Int, Int)

parseCoord :: String -> [Coord]
parseCoord = map (tuplify2 . map read . splitOn ",") . splitOn " -> "

generateRocks :: [[Coord]] -> S.Set Coord
generateRocks = foldr S.insert S.empty . concatMap (concatMap createRockLine . windows 2)
  where createRockLine [(x, y), (x', y')]
          | x == x' = map (x,) [y, y + (signum $ y' - y)..y']
          | otherwise = map (,y) [x, x + (signum $ x' - x)..x']

simSands :: (Int, Bool) -> SQ.Seq Coord -> S.Set Coord -> S.Set Coord
simSands limit@(lowest, stopOnLowest) path s = if SQ.null path || stopOnLowest && (y >= lowest) then s else simSands limit path' s'
  where curPos@(_, y) = SQ.index path 0
        nextPos = filter ((||) stopOnLowest . flip (<) lowest . snd) $ filter (flip S.notMember s) $ map (zipTuple2 (+) curPos) [(0, 1), (-1, 1), (1, 1)]
        (path', s') = if null nextPos then (SQ.drop 1 path, S.insert curPos s) else (head nextPos SQ.<| path, s)

solve :: (Int -> (Int, Bool)) -> S.Set Coord -> Int
solve f s = ((-) `on` S.size) (simSands (f lowestRock) (SQ.singleton (500, 0)) s) s
  where lowestRock = S.findMax $ S.map snd s

solveP1 = solve (, True) . generateRocks . map parseCoord . lines
solveP2 = solve ((, False) . (+) 2) . generateRocks . map parseCoord . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

