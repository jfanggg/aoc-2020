module Main where

import Data.List.Split ( splitOn )
import Data.Maybe ( mapMaybe )
import Data.Ord ( comparing )
import Data.List ( minimumBy )
import Data.Bifunctor ( Bifunctor(second) )

data Input = Input {
  time :: Integer,
  ids :: [String]
} deriving (Show, Eq);


parseInput :: String -> Input
parseInput raw = Input time ids
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
roundN x n = x + n - (x `mod` n)

findEarliest :: Input -> Integer
findEarliest input = minimumBy (comparing f) vals
  where f = roundN (time input)
        vals = mapMaybe parseId (ids input)

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
-- combine (x0, m0) (x1, m1) = (m0, m1, m0', m1')
  where m1' = modInv m1 m0
        m0' = modInv m0 m1
        a = m1' * m1 * x0 + m0' * m0 * x1
        m = m0 * m1

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let input = parseInput raw
  
  let id = findEarliest input
  let wait = roundN (time input) id - time input
  putStr "Part 1: "
  print $ id * wait

  let (h:t) = parseEqs (ids input)
  let (ans2, _) = foldl combine h t 
  putStr "Part 2: "
  print ans2