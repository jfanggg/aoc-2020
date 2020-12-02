module Main where

import Data.List
import Data.List.Split

processLine :: String -> (Int, Int, Char, String)
processLine s = let [x, y, z] = words s
                    [lo, hi] = map read $ splitOn "-" x :: [Int]
                    c = head y
                in (lo, hi, c, z)

validate1 :: (Int, Int, Char, String) -> Int
validate1 (lo, hi, c, z) =  let len = length $ filter (== c) z
                            in fromEnum $ lo <= len && len <= hi

validate2 :: (Int, Int, Char, String) -> Int
validate2 (lo, hi, c, z) = fromEnum $ (z !! (lo - 1) == c) /= (z !! (hi - 1) == c)

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let ls = map processLine $ lines raw
      ans1 = foldr (+) 0 $ map validate1 ls
      ans2 = foldr (+) 0 $ map validate2 ls

  putStr "Part 1: "
  print ans1

  putStr "Part 2: "
  print ans2
