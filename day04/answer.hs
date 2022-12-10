import AOC.IO
import AOC.Utils
import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set

-- part to show
part = 2

-- will submit?
willSubmit = 0

isContains :: [Int] -> Bool
isContains [a, b, c, d] = (a <= c && b >= d) || (c <= a && d >= b)

isOverlaps :: [Int] -> Bool
isOverlaps [a, b, c, d] = a <= d && c <= b

solveP1 = sum . map (fromEnum . isContains . map read . splitOneOf ",-") . lines
solveP2 = sum . map (fromEnum . isOverlaps . map read . splitOneOf ",-") . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

