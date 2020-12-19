module Main where

import Text.Regex.PCRE
import Data.Char (digitToInt, intToDigit)
import Numeric ( showIntAtBase )
import qualified Data.Map as M
import qualified Data.List as L

type Memory = M.Map Integer Integer

data State = State {
  mask :: String,
  memory :: M.Map Integer Integer
} deriving (Eq, Show);

isMask :: String -> Bool
isMask s = s =~ "mask = [01X]{36}"

parseBin :: String -> Integer
parseBin = foldl (\acc c -> 2 * acc + toInteger (digitToInt c)) 0

toBin :: Integer -> String
toBin x = replicate (36 - length bin) '0' ++ bin
  where bin = showIntAtBase 2 intToDigit x ""

updateMask :: State -> String -> State
updateMask (State _ mem) line = State mask mem 
  where mask = drop 7 line

powerset :: [Integer] -> [Integer]
powerset [] = [0]
powerset (x:xs) = [x + p | p <- ps] ++ ps
  where ps = powerset xs

updates1 :: String -> Integer -> Integer -> M.Map Integer Integer 
updates1 mask addr val = M.singleton addr maskedVal
  where maskBit m x = if m == 'X' then x else m
        maskedVal = parseBin $ zipWith maskBit mask (toBin val)

updates2 :: String -> Integer -> Integer -> M.Map Integer Integer 
updates2 mask addr val = updates
  where xVals = map (\x -> 2^(35-x)) $ L.elemIndices 'X' mask
        maskBit m x = case m of 
          '1' -> '1'
          'X' -> '0'
          _   -> x

        baseAddr = parseBin $ zipWith maskBit mask (toBin addr)
        updates = M.fromList $ [(baseAddr + x, val) | x <- powerset xVals]

write :: Bool -> State -> String -> State
write p2 (State mask mem) line = State mask updatedMem
  where capture = line =~ "mem\\[(\\d+)\\] = (\\d+)" :: [[String]]
        [addr, val] = map read (tail $ head capture) :: [Integer]

        updates = (if p2 then updates2 else updates1) mask addr val
        updatedMem = M.union updates mem

executeLine :: Bool -> State -> String -> State 
executeLine p2 state line
  | isMask line = updateMask state line
  | otherwise   = write p2 state line

solve :: [String] -> Bool -> Integer
solve ls p2 = M.foldr (+) 0 mem
  where State _ mem = foldl (executeLine p2) (State "" M.empty) ls

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let ls = lines raw

  let [ans1, ans2] = solve ls <$> [False, True]  
  putStrLn $ "Part 1: " ++ show ans1
  putStrLn $ "Part 2: " ++ show ans2
 