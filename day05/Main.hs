module Main where

decode :: Char -> String -> Int
decode on = foldl (\acc c -> 2 * acc + fromEnum (c == on)) 0

getId :: String -> Int
getId pass = 8 * row + col
  where (first, second) = splitAt 7 pass
        row = decode 'B' first
        col = decode 'R' second

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let input = lines raw

  let ids = map getId input
  let [min, max] = [minimum, maximum] <*> pure ids

  putStr "Part 1: "
  print max

  let isAns = \x -> ((`elem` ids) <$> [x - 1..x + 1]) == [True, False, True] 
  putStr "Part 2: "
  print $ head $ filter isAns [min..max]
  