module Utils (
  printr,
  tuplify2,
  tuplify3,
  tuplify4,
  tuplify5,
  tuplify6,
  splitOn,
  shiftStr
) where

import Data.Char

printr :: String -> IO ()
printr = putStrLn . id

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

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delimiter str = case break (== delimiter) str of
                (a, _delimiter:b)   -> a : splitOn delimiter b
                (a, _empty)         -> [a]

shiftStr :: String -> Int -> String
shiftStr x y = map (\c -> chr $ ord c + y) x

