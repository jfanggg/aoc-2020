module Main where

import Data.List.Split ( splitOn )
import Data.Maybe ( mapMaybe )
import Data.Ord ( comparing )
import Data.List ( minimumBy )
import Data.Bifunctor ( Bifunctor(second) )

parseInput :: String -> (Integer, [String])
parseInput raw = (time, ids)
  where [line1, line2] = lines raw
        time = read line1 :: Integer
        ids = splitOn "," line2

parseId :: String -> Maybe Integer
parseId s = if s == "x" then Nothing else Just (read s)

parseEqs :: [String] -> [(Integer, Integer)]
parseEqs ids = res
  where filtered = filter (\ (_, v) -> v /= "x") (zip [0, -1..] ids)
        res = map (second read) filtered

roundN :: Integer -> Integer -> Integer
roundN x n = x + (-x) `mod` n

findEarliest :: (Integer, [String]) -> Integer
findEarliest (time, ids) = minimumBy (comparing f) vals
  where f = roundN time
        vals = mapMaybe parseId ids

-- finds x, y s.t. ax + by = gcd(a, b)
extendedEuclid :: Integer -> Integer -> (Integer, Integer)
extendedEuclid a b 
  | a `mod` b == 0  = (0, 1)
  | otherwise   = let (q, r) = quotRem a b
                      (x, y) = extendedEuclid b r in
                  (y, x - y * q)

-- finds x s.t. a*x = 1 (mod m)
modInv :: Integer -> Integer -> Integer
modInv a m = x `mod` m
  where (x, _) = extendedEuclid a m

combine :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
combine (x0, m0) (x1, m1) = (a `mod` m, m)
  where m1' = modInv m1 m0
        m0' = modInv m0 m1
        a = m1' * m1 * x0 + m0' * m0 * x1
        m = m0 * m1

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let input@(time, ids) = parseInput raw

  let busId = findEarliest input
  let wait = roundN time busId - time
  putStr "Part 1: "
  print $ busId * wait

  let (h:t) = parseEqs ids
  let (ans2, _) = foldr combine h t 
  putStr "Part 2: "
  print ans2