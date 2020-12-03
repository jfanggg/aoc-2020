module Main where

processLine :: String -> [Int]
processLine = map $ fromEnum . (== '#')

everyNth :: Int -> [a] -> [a]
everyNth n (x:xs) = x : everyNth n (drop (n - 1) xs)
everyNth _ [] = []

travel :: [[Int]] -> Int -> Int -> Int
travel input dr dc = sum collisions
  where n_col = length $ head input
        get_c = \x -> dc * x `mod` n_col
        rows = zip [0..] $ everyNth dr input
        collisions = map (\(idx, row) -> row !! get_c idx) rows

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let input = map processLine $ lines raw

  putStr "Part 1: "
  print $ travel input 1 3

  let slopes = [[1, 1], [1, 3], [1, 5], [1, 7], [2, 1]]
  putStr "Part 2: "
  print $ product $ map (\[dr, dc] -> travel input dr dc) slopes