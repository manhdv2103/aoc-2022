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

compLists :: [String] -> [String] -> Ordering
compLists (a:as) (b:bs)
  | a `elem` ["[", "]"] && b == a = compLists as bs
  | a == "[" && b == "]" = GT
  | a == "]" && b == "[" = LT
  | a == "[" = compLists as $ [b, "]"] ++ bs
  | b == "[" = compLists ([a, "]"] ++ as) bs
  | a == "]" = LT
  | b == "]" = GT
  | otherwise = if compRes == EQ then compLists as bs else compRes
    where compRes = compInts a b

compInts :: String -> String -> Ordering
compInts a b = compare (read a :: Int) (read b :: Int)

parsePacket :: String -> [String]
parsePacket = concatMap (split (dropInitBlank . dropFinalBlank . dropInnerBlanks $ oneOf "[]")) . splitOn ","

dividerPackets :: [String]
dividerPackets = ["[[2]]", "[[6]]"]

solveP1 = sum . map fst . filter ((==) LT . snd) . zip [1..] . map (uncurry compLists . tuplify2) . map2d parsePacket . splitOn [""] . lines
solveP2 = product . map fst . filter (flip elem (map parsePacket dividerPackets) . snd) . zip [1..] . sortBy compLists . map parsePacket . filter (/= "") . (++ dividerPackets) . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

