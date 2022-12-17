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
type HeightMap = V.Vector (V.Vector Char)

getMap :: HeightMap -> Coord -> Maybe Char
getMap m (x, y) = (m V.!? x) >>= (V.!? y)

findNodes :: Char -> HeightMap -> [Coord]
findNodes n = V.toList . V.concat . V.toList . V.filter ((/=) 0 . V.length) . V.imap (\x r -> V.map (\y -> (x, y)) $ V.elemIndices n r)

checkReachable :: HeightMap -> Coord -> Coord -> Maybe Bool
checkReachable m a b = (>=) <$> (height <$> getMap m a) <*> ((pred . height) <$> getMap m b)
 where height :: Char -> Int
       height 'S' = ord 'a'
       height 'E' = ord 'z'
       height c = ord c

bfs :: SQ.Seq (Coord, Int) -> Coord -> S.Set Coord -> HeightMap -> Int
bfs ((curCoord, step) SQ.:<| rest) end visited m = if curCoord == end then step else bfs newSq end newVisited m
  where nextVisits = filter (flip S.notMember visited) $ filter ((==) (Just True) . checkReachable m curCoord) $ map (zipTuple2 (+) curCoord) [(0, 1), (1, 0), (0, -1), (-1, 0)]
        newSq = rest SQ.>< (SQ.fromList $ map (, succ step) nextVisits)
        newVisited = foldr S.insert visited nextVisits

bfs1S m = bfs (SQ.fromList $ map (, 0) s) e (S.fromList s) m
  where s = findNodes 'S' m
        e = head $ findNodes 'E' m

bfsAS m = bfs (SQ.fromList $ map (, 0) s) e (S.fromList s) m
  where s = findNodes 'S' m ++ findNodes 'a' m
        e = head $ findNodes 'E' m

solveP1 = bfs1S . V.fromList . map (V.fromList) . lines
solveP2 = bfsAS . V.fromList . map (V.fromList) . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

