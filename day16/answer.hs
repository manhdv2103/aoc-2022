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
part = 2

-- will submit?
willSubmit = 0

type ValveGraph = HM.HashMap String (Int, HM.HashMap String Int)

inf = 100 :: Int

parseGraph :: [String] -> ValveGraph
parseGraph = HM.fromList . map parseValve
  where parseValve = (\x -> (x!!1, (read (x!!5), parseNeighbors $ drop 11 x))) . splitOneOf " =;,"
        parseNeighbors = HM.fromList . map (,1) . filter (not . null)

getWeight :: String -> String -> ValveGraph -> Int
getWeight s e gr = fromMaybe inf $ snd <$> (HM.lookup s gr) >>= (HM.lookup e)

setWeight :: String -> String -> Int -> ValveGraph -> ValveGraph
setWeight s e w gr = HM.adjust (second (HM.insert e w)) s gr

apsp :: ValveGraph -> ValveGraph
apsp gr = foldr (\v -> flip (foldr $ updateGraph v) valvePairs) gr valves
  where valves = HM.keys gr
        valvePairs = pairs' valves
        updateGraph v (vs, ve) gr
          | w' < (getWeight vs ve gr) = setWeight vs ve w' gr
          | otherwise = gr
          where w' = (getWeight vs v gr) + (getWeight v ve gr)

filterGraph :: ValveGraph -> ValveGraph
filterGraph = HM.filterWithKey (\k (v, _) -> k == "AA" || v > 0)

simulate :: Int -> ValveGraph -> HM.HashMap (HS.HashSet String) Int
simulate m gr = simulate' m 0 "AA" HS.empty ns gr
  where ns = HS.delete "AA" $ HM.keysSet gr
        simulate' m tt cv ps ns gr
          | m <= 0 = HM.empty
          | HS.null ns = st
          | otherwise = foldl' (HM.unionWith max) st $ HS.map branching ns
          where tt' = tt + ((*) m $ fst $ fromJust $ HM.lookup cv gr)
                st = HM.singleton ps tt'
                branching nv = simulate' m' tt' nv (HS.insert nv ps) (HS.delete nv ns) gr
                  where m' = m - (succ $ getWeight cv nv gr)

withElephant :: HM.HashMap (HS.HashSet String) Int -> [Int]
withElephant = map (uncurry ((+) `on` snd)) . filter (HS.null . uncurry (HS.intersection `on` fst)) . pairs . HM.toList

solveP1 = maximum . simulate 30 . filterGraph . apsp . parseGraph . lines
solveP2 = maximum . withElephant . simulate 26 . filterGraph . apsp . parseGraph . lines

main :: IO ()
main = process part solveP1 solveP2 willSubmit

