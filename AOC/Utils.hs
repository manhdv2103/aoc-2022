{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module AOC.Utils (
  Coord,
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
  first3,
  second3,
  third3,
  first4,
  second4,
  third4,
  fourth4,
  fst3,
  snd3,
  trd3,
  fst4,
  snd4,
  trd4,
  fth4,
  shiftChr,
  shiftStr,
  replace,
  windows,
  zip2d,
  zipWith2d,
  map2d,
  f2d,
  getIndices,
  divisible,
  nTimes,
  pairs,
  pairs2,
  between,
  isEven
) where

import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split

type Coord = (Int, Int)

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

first3 :: (a -> d) -> (a, b, c) -> (d, b, c)
first3 f (a, b, c) = (f a, b, c)

second3 :: (b -> d) -> (a, b, c) -> (a, d, c)
second3 f (a, b, c) = (a, f b, c)

third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (a, b, c) = (a, b, f c)

first4 :: (a -> e) -> (a, b, c, d) -> (e, b, c, d)
first4 f (a, b, c, d) = (f a, b, c, d)

second4 :: (b -> e) -> (a, b, c, d) -> (a, e, c, d)
second4 f (a, b, c, d) = (a, f b, c, d)

third4 :: (c -> e) -> (a, b, c, d) -> (a, b, e, d)
third4 f (a, b, c, d) = (a, b, f c, d)

fourth4 :: (d -> e) -> (a, b, c, d) -> (a, b, c, e)
fourth4 f (a, b, c, d) = (a, b, c, f d)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c

fst4 :: (a, b, c, d) -> a
fst4 (a, b, c, d) = a

snd4 :: (a, b, c, d) -> b
snd4 (a, b, c, d) = b

trd4 :: (a, b, c, d) -> c
trd4 (a, b, c, d) = c

fth4 :: (a, b, c, d) -> d
fth4 (a, b, c, d) = d

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
getIndices indices xs = map (xs !!) indices

divisible :: Int -> Int -> Bool
divisible x y = x `rem` y == 0

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f a = foldl (\a' _ -> f a') a [1..n]

pairs :: [a] -> [(a, a)]
pairs ls = [(x, y) | (x:ys) <- tails ls, y <- ys]

pairs2 :: [a] -> [b] -> [(a, b)]
pairs2 a b = [(x,y) | x <- a, y <- b]

between :: Int -> Int -> Int -> Bool
between x z y = ((x <= y) && (y <= z)) || ((z <= y) && (y <= x))

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

