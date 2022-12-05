import AOC.IO
import AOC.Utils
import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec

-- part to show
part = 2

-- will submit?
willSubmit = 0

solve :: String -> [[String]] -> [String]
solve model [stacks, actions] = runActions model actions $ createStacks $ init stacks

createStacks :: [String] -> Vec.Vector String
createStacks = Vec.fromList . map (dropWhile isSpace) . foldr (zipWith (:)) (repeat []) . map (map (flip (!!) 1) . chunksOf 4)

parseAction :: String -> [Int]
parseAction = updateIndices . map read . filter (all isDigit) . splitOn " "
  where updateIndices [amt, from, to] = [amt, from - 1, to - 1]

craneMove :: String -> String -> String
craneMove model stack
  | model == "9001" = stack
  | otherwise = reverse stack -- properly those old CrateMover 9000

runActions :: String -> [String] -> Vec.Vector String -> [String]
runActions model actions stacks = Vec.toList $ foldl' (\s a -> updateStacks s $ parseAction a) stacks actions
  where
    updateStacks :: Vec.Vector String -> [Int] -> Vec.Vector String
    updateStacks s [amt, from, to] = s Vec.// [(from, drop amt $ s Vec.! from), (to, (craneMove model $ take amt $ s Vec.! from) ++ (s Vec.! to))]

solveP1 = map head . solve "9000" . splitOn ([""]) . lines
solveP2 = map head . solve "9001" . splitOn ([""]) . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

