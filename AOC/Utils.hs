{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module AOC.Utils (
  prettyShow,
  prettyPrint,
  prettyPrintLn,
  tuplify2,
  tuplify3,
  tuplify4,
  mapTuple2,
  mapTuple3,
  mapTuple4,
  zipTuple2,
  zipTuple3,
  zipTuple4,
  first,
  second,
  shiftChr,
  shiftStr,
  replace,
  windows,
  zip2d,
  zipWith2d,
  map2d,
  f2d,
  getIndices,
  divisible
) where

import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split

trimQuotes :: String -> String
trimQuotes s
  | "\"" `isSuffixOf` s = fromMaybe s $ stripPrefix "\"" sTrimHead 
  | otherwise = s
  where sTrimHead = init s

prettyShow :: Show a => a -> String
prettyShow = replace "\\n" "\n" . trimQuotes . show

prettyPrint :: Show a => a -> IO ()
prettyPrint = putStr . prettyShow

prettyPrintLn :: Show a => a -> IO ()
prettyPrintLn = putStrLn . prettyShow

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)

tuplify4 :: [a] -> (a,a,a,a)
tuplify4 [x,y,z,a] = (x,y,z,a)

mapTuple2 :: (a -> b) -> (a,a) -> (b,b)
mapTuple2 f (x,y) = (f x,f y)

mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3 f (x,y,z) = (f x,f y,f z)

mapTuple4 :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
mapTuple4 f (x,y,z,a) = (f x,f y,f z,f a)

zipTuple2 :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
zipTuple2 f (a,b) (a',b')  = (f a a', f b b')

zipTuple3 :: (a -> b -> c) -> (a,a,a) -> (b,b,b) -> (c,c,c)
zipTuple3 f (a,b,c) (a',b',c')  = (f a a', f b b', f c c')

zipTuple4 :: (a -> b -> c) -> (a,a,a,a) -> (b,b,b,b) -> (c,c,c,c)
zipTuple4 f (a,b,c,d) (a',b',c',d')  = (f a a', f b b', f c c', f d d')

first :: (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

shiftChr :: Char -> Int -> Char
shiftChr c y = chr $ ord c + y

shiftStr :: String -> Int -> String
shiftStr x y = map (flip shiftChr y) x

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

windows :: Int -> [a] -> [[a]]
windows n = divvy n 1

zip2d :: [[a]] -> [[b]] -> [[(a, b)]]
zip2d = zipWith zip

zipWith2d :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2d f = zipWith (zipWith f)

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

f2d :: ([a] -> a) -> [[a]] -> a
f2d f = f . map f

getIndices :: [Int] -> [a] -> [a]
getIndices indices = map snd . filter (flip elem indices . fst) . zip [0..]

divisible :: Int -> Int -> Bool
divisible x y = x `rem` y == 0

