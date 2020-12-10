module Main where

import Data.List ( sort )
import qualified Data.Vector as V
import qualified Data.Map as M

countPaths :: M.Map Int Int -> Int -> M.Map Int Int
countPaths dp key = updated
  where val = M.findWithDefault 0 key dp
        
        updateKeys = [key + 1 .. key + 3]
        updateVals = map (val+) $ M.findWithDefault 0 <$> updateKeys <*> pure dp

        updated = M.fromList $ zip (key:updateKeys) (val:updateVals)

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let nums = map read $ lines raw :: [Int]

  let end = maximum nums + 3
  let sorted = 0 : sort nums ++ [end]

  let diffs = zipWith (-) (tail sorted) sorted
  let ones   = length $ filter (== 1) diffs
  let threes = length $ filter (== 3) diffs

  putStr "Part 1: "
  print $ ones * threes

  let dp = foldl countPaths (M.singleton 0 1) (V.fromList sorted)
  putStr "Part 2: "
  print $ dp M.! end
