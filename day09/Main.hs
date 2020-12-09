module Main where

import Text.Regex.PCRE
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List ( tails )

preambleLength :: Int
preambleLength = 25

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

check :: [Int] -> Int -> Bool
check nums index = val `elem` [x + y | (x:xs) <- tails preceding, y <- xs]
  where val = nums !! index
        preceding = slice (index - preambleLength) (index - 1) nums

findRange :: [Int] -> Int -> Int
findRange nums target = minimum r + maximum r
  where rs = [slice s e nums | s <- [0..length nums - 1], e <- [s + 1 .. length nums - 1]]
        r = head $ filter (\x -> sum x == target) rs

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let nums = map read $ lines raw :: [Int]

  let idx = head $ filter (not . check nums) [preambleLength..length nums - 1]
  let ans1 = nums !! idx
  putStr "Part 1: "
  print ans1

  putStr "Part 2: "
  print $ findRange nums ans1