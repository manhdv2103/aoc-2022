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

getPriority :: Char -> Int
getPriority c = if isLower c then oc - (ord 'a') + 1 else oc - (ord 'A') + 27
  where oc = ord c

findDuplicates :: (String, String) -> String
findDuplicates (c1, c2) = (filter (flip Set.member c1Set) c2)
  where c1Set = Set.fromList c1

findDuplicates3 :: [String] -> String
findDuplicates3 [r1, r2, r3] = findDuplicates (findDuplicates (r1, r2), r3)

solveP1 = sum . map (\r -> getPriority $ flip (!!) 0 $ findDuplicates $ splitAt (length r `div` 2) r) . lines
solveP2 = sum . map (getPriority . flip (!!) 0 . findDuplicates3) . chunksOf 3 . lines

main :: IO ()
main = process part solveP1 solveP2

