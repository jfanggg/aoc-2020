module Main where

getRow :: String -> Int
getRow = foldl (\acc c -> 2 * acc + fromEnum (c == 'B')) 0

getCol :: String -> Int
getCol = foldl (\acc c -> 2 * acc + fromEnum (c == 'R')) 0

getId :: String -> Int
getId pass = 8 * row + col
  where (first, second) = splitAt 7 pass
        row = getRow first
        col = getCol second

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let input = lines raw

  putStr "Part 1: "
  print $ maximum $ map getId input