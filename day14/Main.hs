module Main where

import Text.Regex.PCRE
import Data.Char (digitToInt, intToDigit)
import Numeric ( showIntAtBase )
import qualified Data.Map as M

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

write :: State -> String -> State
write (State mask mem) line = State mask mem'
  where capture = line =~ "mem\\[(\\d+)\\] = (\\d+)" :: [[String]]
        [addr, val] = map read (tail $ head capture) :: [Integer]

        f m x = if m == 'X' then x else m
        masked = parseBin $ zipWith f mask (toBin val)
        mem' = M.insert addr masked mem

execute :: State -> String -> State 
execute state line
  | isMask line = updateMask state line
  | otherwise   = write state line

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let ls = lines raw
  
  let State _ mem1 = foldl execute (State "" M.empty) ls
  putStr "Part 1: "
  print $ M.foldr (+) 0 mem1