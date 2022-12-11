import AOC.IO
import AOC.Utils
import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

-- part to show
part = 2

-- will submit?
willSubmit = 0

data Monke = Monke {
  items :: [Int],
  operation :: (String, String),
  testNum :: Int,
  destinations :: (Int, Int),
  totalIns :: Int
} deriving (Show, Read, Eq)

parseMonke :: [String] -> Monke
parseMonke monke = Monke { items = items, operation = op, testNum = test, destinations = des, totalIns = 0 }
  where items = map read $ splitOn ", " $ drop 18 $ monke!!1
        op = tuplify2 $ words $ drop 23 $ monke!!2
        test = read $ drop 21 $ monke!!3
        des = (read $ drop 29 $ monke!!4, read $ drop 30 $ monke!!5)

monkeOperate :: Bool -> Int -> (String, String) -> Int -> Int
monkeOperate highWorry magicNumber (operator, target) old = op old tg `mod` magicNumber `div` if highWorry then 1 else 3
  where op = case operator of
              "+" -> (+)
              "*" -> (*)
        tg = case target of
              "old" -> old
              x -> read x

monkePlay :: Bool -> Int -> V.Vector Monke -> Int -> V.Vector Monke
monkePlay highWorry magicNumber allMonke idx = allMonke V.// [(idx, newMonke), (d1, desMonke1), (d2, desMonke2)]
  where monke = allMonke V.! idx
        newMonke = monke {items = [], totalIns = totalIns monke + (length $ items monke)}
        (d1, d2) = destinations monke
        throwItems = partition (flip divisible $ testNum monke) $ map (monkeOperate highWorry magicNumber $ operation monke) $ items monke
        (desMonke1, desMonke2) = zipTuple2 (\i m -> m { items = items m ++ i }) throwItems $ mapTuple2 ((V.!) allMonke) $ destinations monke

monkeRound :: Bool -> V.Vector Monke -> Int -> V.Vector Monke
monkeRound highWorry allMonke _ = foldl' (monkePlay highWorry magicNumber) allMonke [0..(V.length allMonke - 1)]
  where magicNumber = V.product $ V.map testNum allMonke

solveP1 xs = product $ take 2 $ sortBy (flip compare) $ V.toList $ V.map totalIns $ foldl' (monkeRound False) (V.fromList $ map parseMonke $ splitOn [""] $ lines xs) [1..20]
solveP2 xs = product $ take 2 $ sortBy (flip compare) $ V.toList $ V.map totalIns $ foldl' (monkeRound True) (V.fromList $ map parseMonke $ splitOn [""] $ lines xs) [1..10000]

main :: IO ()
main = process part solveP1 solveP2 willSubmit

