module Main where

import Data.List.Split ( splitOn )

processLine :: String -> (Int, Int, Char, String)
processLine s = (lo, hi, c, z)
  where [x, y, z] = words s
        [lo, hi] = map read $ splitOn "-" x :: [Int]
        c = head y

validate1 :: (Int, Int, Char, String) -> Int
validate1 (lo, hi, c, s) = fromEnum $ lo <= len && len <= hi
  where len = length $ filter (== c) s

validate2 :: (Int, Int, Char, String) -> Int
validate2 (lo, hi, c, s) = fromEnum $ (s !! (lo - 1) == c) /= (s !! (hi - 1) == c)

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let ls = map processLine $ lines raw

  let f = \x -> sum . map x
  let [ans1, ans2] = f <$> [validate1, validate2] <*> pure ls

  putStr "Part 1: "
  print ans1

  putStr "Part 2: "
  print ans2
