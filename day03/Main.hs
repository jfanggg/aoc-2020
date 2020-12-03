module Main where

processLine :: String -> [Bool]
processLine = map (== '#')

travel :: [[Bool]] -> Int
travel m = sum $ map (\(r, row) -> fromEnum $ row !! get_c r) rows
  where c' = length $ head m
        get_c = \r -> 3 * r `mod` c'
        rows = zip [0..] m

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let ls = map processLine $ lines raw

  putStr "Part 1: "
  print $ travel ls