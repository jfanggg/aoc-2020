module Main where

import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.List

data Input = Input {
  time :: Int,
  ids :: [Int]
} deriving (Show, Eq);

parseId :: String -> Maybe Int
parseId s = if s == "x" then Nothing else Just (read s)

parseInput :: String -> Input
parseInput raw = Input time ids
  where [line1, line2] = lines raw
        time = read line1 :: Int
        vals = splitOn "," line2
        ids = mapMaybe parseId vals

roundN :: Int -> Int -> Int
roundN x n = x + n - (x `mod` n)

findEarliest :: Input -> Int
findEarliest input = minimumBy (comparing f) (ids input)
  where f = roundN (time input)

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let input = parseInput raw
  let id = findEarliest input
  let wait = roundN (time input) id - time input

  putStr "Part 1: "
  print $ id * wait