import AOC
import Utils
import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import qualified Data.Map as Map

-- part to show
part = 2

solveP1 = maximum . map (sum . map read) . splitOn [""] . lines
solveP2 = sum . take 3 . sortBy (flip compare) . map (sum . map read) . splitOn [""] . lines

main :: IO ()
main = process part solveP1 solveP2

