{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

import AOC.IO
import AOC.Utils
import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import Data.Maybe
import Debug.Trace
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Sequence as SQ
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

-- part to show
part = 1

-- will submit?
willSubmit = 0

solveP1 xs = 0
solveP2 xs = 0

main :: IO ()
main = process part solveP1 solveP2 willSubmit

