{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module AOC.Utils (
  prettyShow,
  prettyPrint,
  prettyPrintLn,
  tuplify2,
  tuplify3,
  tuplify4,
  tuplify5,
  tuplify6,
  mapTuple2,
  mapTuple3,
  mapTuple4,
  mapTuple5,
  mapTuple6,
  shiftChr,
  shiftStr,
  replace
) where

import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split

firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

trimQuotes :: String -> String
trimQuotes s
  | "\"" `isSuffixOf` s = fromMaybe s $ stripPrefix "\"" sTrimHead 
  | otherwise = s
  where sTrimHead = init s

prettyShow :: Show a => a -> String
prettyShow = trimQuotes . show

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

tuplify5 :: [a] -> (a,a,a,a,a)
tuplify5 [x,y,z,a,b] = (x,y,z,a,b)

tuplify6 :: [a] -> (a,a,a,a,a,a)
tuplify6 [x,y,z,a,b,c] = (x,y,z,a,b,c)

mapTuple2 :: (a -> b) -> (a,a) -> (b,b)
mapTuple2 f (x,y) = (f x,f y)

mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3 f (x,y,z) = (f x,f y,f z)

mapTuple4 :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
mapTuple4 f (x,y,z,a) = (f x,f y,f z,f a)

mapTuple5 :: (a -> b) -> (a,a,a,a,a) -> (b,b,b,b,b)
mapTuple5 f (x,y,z,a,b) = (f x,f y,f z,f a,f b)

mapTuple6 :: (a -> b) -> (a,a,a,a,a,a) -> (b,b,b,b,b,b)
mapTuple6 f (x,y,z,a,b,c) = (f x,f y,f z,f a,f b,f c)

shiftChr :: Char -> Int -> Char
shiftChr c y = chr $ ord c + y

shiftStr :: String -> Int -> String
shiftStr x y = map (flip shiftChr y) x

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

