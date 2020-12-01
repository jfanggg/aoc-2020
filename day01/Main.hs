module Main where

import Data.List

-- all unique pairs of elements from a given list
pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:t) <- tails l, y <- t]

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let nums = map read $ lines raw :: [Int]
  let (x, y) = (filter (\(x, y) -> x + y == 2020) $ pairs nums) !! 0
  putStr "Part 1: "
  print (x * y)
