module Main where

import Data.List
import Text.Regex.PCRE
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Map as M

-- backtrack :: V.Vector Int -> Int -> Int
-- backtrack nums idx = ans
--   where done = idx == length nums - 1
--         val = nums V.! idx

--         ans
--           | done = 1
--           | otherwise = sum $ map (backtrack nums) $ takeWhile (\x -> nums V.! x - val <= 3) [idx + 1 .. length nums - 1]


p2 :: V.Vector Int -> M.Map Int Int -> Int -> M.Map Int Int
p2 nums oldMap idx = newMap
  where val = nums V.! idx
        zer = if val `M.member` oldMap then oldMap M.! val else 1
        one = if (val + 1) `M.member` oldMap then oldMap M.! (val + 1) else 0
        two = if (val + 2) `M.member` oldMap then oldMap M.! (val + 2) else 0
        thr = if (val + 3) `M.member` oldMap then oldMap M.! (val + 3) else 0

        newMap'' = M.insert (val + 1) (zer + one) oldMap
        newMap' = M.insert (val + 2) (zer + two) newMap''
        newMap = M.insert (val + 3) (zer + thr) newMap'

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let nums = map read $ lines raw :: [Int]

  let sorted = V.fromList $ sort nums
  let diffs = map (\idx -> sorted V.! (idx + 1) - sorted V.! idx) [0..length sorted - 2]

  let ones = length $ filter (== 1) diffs
  let threes = length $ filter (== 3) diffs

  putStr "Part 1: "
  print $ (ones +1) * (threes+1)

  let end = maximum nums + 3
  let list2 = V.fromList $ [0] ++ sort nums ++ [end]

  putStr "Part 2: "
  print $ (foldl (p2 list2) M.empty [0..length list2 - 1]) M.! end
