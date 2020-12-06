module Main where

import Data.List.Split ( splitOn )
import qualified Data.Set as S

numQuestions :: String -> Int
numQuestions s = S.size $ S.fromList filtered
  where filtered = filter (/= ' ') s

allAnswered :: String -> Int
allAnswered s = S.size (foldl S.intersection (S.fromList ['a'..'z']) sets)
  where responses = words s
        sets = map S.fromList responses

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let groups = map unwords $ splitOn [""] $ lines raw

  putStr "Part 1: "
  print $ sum $ map numQuestions groups

  putStr "Part 2: "
  print $ sum $ map allAnswered groups