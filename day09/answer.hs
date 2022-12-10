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
import Data.Int

-- part to show
part = 2

-- will submit?
willSubmit = 0

type Knot = (Int, Int)
type Rope = [Knot]

expandMove :: [Char] -> [Char]
expandMove (dir:num) = replicate (read $ num) dir

moveKnot :: Knot -> Knot -> Knot
moveKnot (hx, hy) (tx, ty) = if shouldMove then (tx + signum dx, ty + signum dy) else (tx, ty)
  where shouldMove = abs dx > 1 || abs dy > 1
        dx = hx - tx
        dy = hy - ty

moveRope :: Rope -> Char -> Rope
moveRope (rHead:rBody) cmd = scanl' moveKnot newHead $ rBody
  where newHead = case cmd of
          'R' -> (succ $ fst rHead, snd rHead)
          'L' -> (pred $ fst rHead, snd rHead)
          'U' -> (fst rHead, succ $ snd rHead)
          'D' -> (fst rHead, pred $ snd rHead)

solveP1 = S.size . S.fromList . map last . scanl' moveRope (replicate 2 (0, 0)) . concatMap expandMove . lines
solveP2 = S.size . S.fromList . map last . scanl' moveRope (replicate 10 (0, 0)) . concatMap expandMove . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

