module Main where

import Data.List ( tails )
import Data.Maybe ( mapMaybe )

preambleLen :: Int
preambleLen = 25

minInt :: Int
minInt = minBound

maxInt :: Int
maxInt = maxBound

checkWindow :: [Int] -> Maybe Int
checkWindow window = if not valid then Just h else Nothing
  where (h:preamble) = reverse window
        valid = h `elem` [x + y | (x:xs) <- tails preamble, y <- xs]

findRange :: Int -> Int -> Int -> Int -> [Int] -> Maybe Int
findRange target sumAcc minAcc maxAcc (h:t)  = res
  where sumAcc' = sumAcc + h
        minAcc' = min minAcc h
        maxAcc' = max maxAcc h
        res 
          | sumAcc' == target = Just (maxAcc' + minAcc')
          | sumAcc' >  target = Nothing
          | otherwise         = findRange target sumAcc' minAcc' maxAcc' t

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let nums = map read $ lines raw :: [Int]

  let windows = map (take $ preambleLen + 1) $ take (length nums - preambleLen) $ tails nums
  let ans1 = head $ mapMaybe checkWindow windows
  putStr "Part 1: "
  print ans1

  let ans2 = head $ mapMaybe (findRange ans1 0 maxInt minInt) $ tails nums
  putStr "Part 2: "
  print ans2