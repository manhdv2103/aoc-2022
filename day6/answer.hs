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

solve :: Int -> String -> Int
solve n = (+ n) . length . takeWhile ((/=) n . length . nub) . windows n

solveP1 = solve 4
solveP2 = solve 14

main :: IO ()
main = process part solveP1 solveP2 willSubmit

