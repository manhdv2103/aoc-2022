import AOC.IO
import AOC.Utils
import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

-- part to show
part = 2

-- will submit?
willSubmit = 0 

expandStmt :: String -> [Int]
expandStmt stmt = if "addx" `isPrefixOf` stmt then [0, read $ drop 5 stmt] else [0]

draw :: Int -> Int -> Char
draw pos sprite = if abs (pos `mod` 40 - sprite) <= 1 then '#' else '.'

solveP1 = sum . getIndices (map pred [20,60..220]) . zipWith (*) [1..] . scanl' (+) 1 . concat . map expandStmt . lines
solveP2 = unlines . chunksOf 40 . zipWith draw [0..] . init . scanl' (+) 1 . concat . map expandStmt . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

