import AOC.IO
import AOC.Utils
import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

-- part to show
part = 2

-- will submit?
willSubmit = 0

constructStacks :: [String] -> V.Vector String
constructStacks = V.fromList . map (dropWhile isSpace) . transpose . map (map head . chunksOf 4 . tail)

craneMove :: String -> String -> String
craneMove model stack
  | model == "9001" = stack
  | otherwise = reverse stack -- properly those old CrateMover 9000s

parseAction :: String -> [Int]
parseAction = updateIndices . map read . filter (all isDigit) . splitOn " "
  where updateIndices [amt, from, to] = [amt, from - 1, to - 1]

runActions :: String -> [String] -> V.Vector String -> [String]
runActions model actions stacks = V.toList $ foldl' updateStacks stacks $ map parseAction actions
  where updateStacks s [amt, from, to] = s V.// [(from, stay), (to, (craneMove model move) ++ (s V.! to))]
          where (move, stay) = splitAt amt $ s V.! from

craneRun :: String -> [[String]] -> [String]
craneRun model [stackRows, actions] = runActions model actions $ constructStacks $ init stackRows

solveP1 = map head . craneRun "9000" . splitOn ([""]) . lines
solveP2 = map head . craneRun "9001" . splitOn ([""]) . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

