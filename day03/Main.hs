module Main where

import Data.List.Split ( chunksOf )

processLine :: String -> [Bool]
processLine = map (== '#')

travel :: [[Bool]] -> Int -> Int -> Int
travel m dr dc = sum $ map (\(idx, row) -> fromEnum $ row !! get_c idx) rows
  where c' = length $ head m
        get_c = \x -> dc * x `mod` c'
        rows = zip [0..] $ map head $ chunksOf dr m

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let ls = map processLine $ lines raw

  putStr "Part 1: "
  print $ travel ls 1 3

  putStr "Part 2: "
  print $ product [travel ls 1 1, travel ls 1 3, travel ls 1 5, travel ls 1 7, travel ls 2 1]