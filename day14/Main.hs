module Main where

import Text.Regex.PCRE
import Data.Char (digitToInt)
import Data.Bits
import qualified Data.Map as M

type Memory = M.Map Integer Integer

data State = State {
  mask0 :: Integer,
  mask1 :: Integer,
  memory :: M.Map Integer Integer
} deriving (Eq, Show);

isMask :: String -> Bool
isMask s = s =~ "mask = [01X]{36}"

parseBin :: String -> Integer
parseBin = foldl (\acc c -> 2 * acc + toInteger (digitToInt c)) 0

updateMask :: State -> String -> State
updateMask (State _ _ mem) mask = State m0' m1' mem 
  where m0' = parseBin $ map (\c -> if c == '0' then '0' else '1') mask
        m1' = parseBin $ map (\c -> if c == '1' then '1' else '0') mask

writeVal :: State -> String -> State
writeVal (State m0 m1 mem) line = State m0 m1 mem'
  where capture = line =~ "mem\\[(\\d+)\\] = (\\d+)" :: [[String]]
        (_:t) = head capture 
        [addr, val] = map read t :: [Integer]
        masked = (val .|. m1) .&. m0
        mem' = M.insert addr masked mem

execute :: State -> String -> State 
execute state line
  | isMask line = updateMask state line
  | otherwise   = writeVal state line

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let ls = lines raw
  
  let state1 = foldl execute (State 0 0 M.empty) ls
  putStr "Part 1: "
  print $ M.foldr (+) 0 (memory state1)