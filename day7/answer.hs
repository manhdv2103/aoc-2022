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

type Tree = M.Map [String] Int
type TreeState = ([String], Tree)

constructTree :: TreeState -> String -> TreeState
constructTree (path, tree) cmd
  | isPrefixOf "$ ls" cmd || isPrefixOf "dir" cmd = (path, tree)
  | isPrefixOf "$ cd .." cmd = (tail path, tree)
  | isPrefixOf "$ cd" cmd = let 
      dir = drop 5 cmd
      newPath = dir:path
    in (newPath, M.insert newPath 0 tree)
  | otherwise = let
      size = read $ head $ words $ cmd :: Int
    in (path, M.adjust (+ size) path tree)

updateTreeSize :: TreeState -> [Int]
updateTreeSize (_, tree) = M.elems $ foldl' (\t path -> M.insertWith (+) (tail path) (t M.! path) t) tree $ sortBy (flip compare `on` length) $ M.keys tree

findDeleteSize :: [Int] -> Int
findDeleteSize xs = minimum $ filter (\s -> s >= 30000000 - (70000000 - last sortedXs)) sortedXs
  where sortedXs = sort xs

solveP1 = sum . filter (<= 100000) . updateTreeSize . foldl' constructTree ([], M.empty) . lines
solveP2 = findDeleteSize . updateTreeSize . foldl' constructTree ([], M.empty) . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

