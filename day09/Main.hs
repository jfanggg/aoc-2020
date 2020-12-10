module Main where

import Data.List ( tails )
import Data.Maybe ( mapMaybe )
import qualified Data.Vector as V

preambleLen :: Int
preambleLen = 25

checkWindow :: [Int] -> Maybe Int
checkWindow window = if not valid then Just h else Nothing
  where (h:preamble) = reverse window
        valid = h `elem` [x + y | (x:xs) <- tails preamble, y <- xs]

findRange :: V.Vector Int -> Int -> Int
findRange nums target = V.maximum range + V.minimum range
  where find s e acc 
          | acc == target = V.slice s (e - s) nums
          | acc >  target = find (s + 1) e (acc - nums V.! s)
          | otherwise     = find s (e + 1) (acc + nums V.! e)
        
        range = find 0 0 0

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let nums = map read $ lines raw :: [Int]

  let windows = map (take $ preambleLen + 1) $ take (length nums - preambleLen) $ tails nums
  let ans1 = head $ mapMaybe checkWindow windows
  putStr "Part 1: "
  print ans1

  putStr "Part 2: "
  print $ findRange (V.fromList nums) ans1