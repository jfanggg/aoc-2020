module Main where

import Data.List.Split ( splitOn )
import qualified Data.Set as S

combine :: (S.Set Char -> S.Set Char -> S.Set Char) -> String -> Int
combine f s = S.size $ foldl f h t
  where (h:t) = map S.fromList $ words s

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let groups = map unwords $ splitOn [""] $ lines raw

  let f = \x -> sum . map (combine x)
  let [ans1, ans2] = f <$> [S.union, S.intersection] <*> pure groups

  putStr "Part 1: "
  print ans1

  putStr "Part 2: "
  print ans2