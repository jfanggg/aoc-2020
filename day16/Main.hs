module Main where

import Text.Regex.PCRE
import Data.List.Split

type Range = (Int, Int)


parseFields :: String -> [Range]
parseFields line = ranges
  where capture = line =~ "[a-z ]+: (\\d+)-(\\d+) or (\\d+)-(\\d+)" :: [[String]]
        ranges = capture 
                  >>= (\x -> [map read $ tail x] :: [[Int]]) 
                  >>= (\[a, b, c, d] -> [(a, b), (c, d)])

inRange :: Int -> Range -> Bool
inRange v (lo, hi) = lo <= v && v <= hi

countInvalid :: [[Range]] -> String -> Int
countInvalid ranges ticket = sum invalid
  where vals = map read $ splitOn "," ticket :: [Int]
        invalid = filter (\v -> not $ any (any (inRange v)) ranges) vals

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let ls = lines raw

  let [fields, _, nearby] = splitOn [""] ls

  let ranges = filter (/= []) $ map parseFields fields
  let invalidSum = sum $ map (countInvalid ranges) (tail nearby)

  putStrLn $ "Part 1: " ++ show invalidSum