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

mapTrees :: (a -> a -> a) -> (Int -> [Int] -> a) -> [[Int]] -> [[a]]
mapTrees mergeOp processTree rows = zipWith2d mergeOp (transpose $ processTrees cols) $ processTrees rows
  where cols = transpose rows
        processTrees = map (map (\(tree, surroundingTrees) -> uncurry (on mergeOp $ processTree tree) surroundingTrees) . lookAround)
        lookAround treeLine = zip treeLine $ zip (map reverse $ inits treeLine) $ tails $ tail treeLine

isVisible :: Int -> [Int] -> Bool
isVisible t = all (< t)

scenicScore :: Int -> [Int] -> Int
scenicScore t ts = (fromEnum $ length ts /= shortTrees) + shortTrees
  where shortTrees = length $ takeWhile (< t) ts

solveP1 = f2d sum . map2d fromEnum . mapTrees (||) isVisible . map2d digitToInt . lines
solveP2 = f2d maximum . mapTrees (*) scenicScore . map2d digitToInt . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

