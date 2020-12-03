module Main where

import Data.List.Split ( chunksOf )

processLine :: String -> [Int]
processLine = map $ fromEnum . (== '#')

travel :: [[Int]] -> Int -> Int -> Int
travel input dr dc = sum collisions
  where n_col = length $ head input
        get_c = \x -> dc * x `mod` n_col
        rows = zip [0..] $ map head $ chunksOf dr input
        collisions = map (\(idx, row) -> row !! get_c idx) rows

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let ls = map processLine $ lines raw

  putStr "Part 1: "
  print $ travel ls 1 3

  let slopes = [[1, 1], [1, 3], [1, 5], [1, 7], [2, 1]]
  putStr "Part 2: "
  print $ product $ map (\[dr, dc] -> travel ls dr dc) slopes