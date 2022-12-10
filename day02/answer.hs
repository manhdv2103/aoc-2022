import AOC.IO
import AOC.Utils
import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import qualified Data.Map as Map

-- part to show
part = 2

-- will submit?
willSubmit = 0

-- Game selections: 0 (Rock), 1 (Paper), 2 (Scissors)
-- Game results (for me): 0 (Lose), 1 (Draw), 2 (Win)

play :: [Int] -> Int
play [op, me] = (me - op + 1) `mod` 3

getMe :: [Int] -> Int
getMe [op, res] = (op + res - 1) `mod` 3

normalize :: Char -> Char
normalize c = if elem c "XYZ" then shiftChr c (-23) else c

convert :: String -> Int
convert s = (ord $ normalize $ s!!0) - ord 'A'

gameScore :: Int -> Int
gameScore = (*) 3

selScore :: Int -> Int
selScore = (+) 1

solveP1 = sum . map (\x -> (gameScore $ play x) + (selScore $ x!!1)) . map (map convert . splitOn " ") . lines
solveP2 = sum . map (\x -> let me = getMe x in (gameScore $ play [x!!0, me]) + selScore me) . map (map convert . splitOn " ") . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

