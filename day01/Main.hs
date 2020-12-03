module Main where

import Data.List ( tails )

pairs :: [a] -> [[a]]
pairs l = [[x, y] | (x:xs) <- tails l, y <- xs]

triples :: [a] -> [[a]]
triples l = [[x, y, z] | (x:xs) <- tails l, (y:ys) <- tails xs, z <- ys]

solve :: [[Int]] -> Int
solve l = product ans
  where ans = head $ filter (\x -> sum x == 2020) l

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let nums = map read $ lines raw :: [Int]

  putStr "Part 1: "
  print $ solve $ pairs nums

  putStr "Part 2: "
  print $ solve $ triples nums
