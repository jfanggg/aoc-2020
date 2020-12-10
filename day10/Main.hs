module Main where

import Data.List ( sort )
import qualified Data.Vector as V
import qualified Data.Map as M

countPaths :: M.Map Int Int -> Int -> M.Map Int Int
countPaths dp num = dpUpdated
  where val = M.findWithDefault 1 num dp
        
        updateKeys = [num + 1 .. num + 3]
        updateVals = map (val+) $ M.findWithDefault 0 <$> updateKeys <*> pure dp

        updates = M.fromList $ zip updateKeys updateVals
        dpUpdated = M.union updates dp

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let nums = map read $ lines raw :: [Int]

  let end = maximum nums + 3
  let sorted = V.fromList $ [0] ++ sort nums ++ [end]

  let diffs = map (\idx -> sorted V.! (idx + 1) - sorted V.! idx) [0..length sorted - 2]
  let ones   = length $ filter (== 1) diffs
  let threes = length $ filter (== 3) diffs

  putStr "Part 1: "
  print $ ones * threes

  let dp = foldl countPaths M.empty sorted
  putStr "Part 2: "
  print $ dp M.! end
