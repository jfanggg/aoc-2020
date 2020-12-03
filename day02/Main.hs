module Main where

import Data.List.Split ( splitOn )

processLine :: String -> (Int, Int, Char, String)
processLine s = (num1, num2, c, pw)
  where [x, c:_, pw] = words s
        [num1, num2] = map read $ splitOn "-" x :: [Int]

validate1 :: (Int, Int, Char, String) -> Int
validate1 (lo, hi, c, pw) = fromEnum $ lo <= len && len <= hi
  where len = length $ filter (== c) pw

validate2 :: (Int, Int, Char, String) -> Int
validate2 (idx1, idx2, c, pw) = fromEnum $ (pw !! (idx1 - 1) == c) /= (pw !! (idx2 - 1) == c)

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let input = map processLine $ lines raw

  let f = \x -> sum . map x
  let [ans1, ans2] = f <$> [validate1, validate2] <*> pure input

  putStr "Part 1: "
  print ans1

  putStr "Part 2: "
  print ans2
