module Main where

import Text.Regex.PCRE
import Data.List ( isPrefixOf, transpose )
import Data.List.Split ( splitOn )
import Data.Maybe ( isNothing, mapMaybe )
import Debug.Trace

type Range = (Int, Int)
data Rule = Rule {
  name :: String,
  ranges :: [Range]
} deriving (Show, Eq);

parseCapture :: [String] -> Maybe Rule
parseCapture xs = Just $ Rule name [(a, b), (c, d)]
  where _:name:vals = xs
        [a, b, c, d] = map read vals :: [Int]

parseFields :: String -> Maybe Rule
parseFields line = rule
  where capture = line =~ "([a-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)" :: [[String]]
        rule = if null capture then Nothing else parseCapture (head capture)

check :: Rule -> Int -> Bool
check (Rule _ rs) val = any (\ (lo, hi) -> lo <= val && val <= hi) rs

checkRow :: [Rule] -> [Int] -> Maybe Int
checkRow rules row = if null invalid then Nothing else Just $ sum invalid
  where invalid = filter (\v -> not $ any (`check` v) rules) row

matrixify :: [String] -> [[Int]]
matrixify = map (map read . splitOn ",")

getValidRules :: [Rule] -> [Int] -> [Rule]
getValidRules rules group = goodRules
  where goodRules = filter (\r -> all (check r) group) rules

deduce :: [[Rule]] -> [[Rule]]
deduce old = if old == new then new else deduce new
  where confirmed = map (name . head) $ filter (\g -> length g == 1) old
        new = map (\g -> if length g == 1 then g else filter (\r -> name r `notElem` confirmed) g) old

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let ls = lines raw

  let [fields, _:ticket, _:nearbyLines] = splitOn [""] ls

  let rules = mapMaybe parseFields fields
  let nearby = matrixify nearbyLines

  let invalidSum = sum $ mapMaybe (checkRow rules) nearby
  putStrLn $ "Part 1: " ++ show invalidSum
  
  let valid = filter (isNothing . checkRow rules) nearby
  let groups = transpose valid

  let possibilities = map (getValidRules rules) groups
  let deduced = deduce possibilities

  let ticketVals = head $ matrixify ticket
  let idxs = map snd $ filter ( \([Rule n _], _) -> "departure" `isPrefixOf ` n) $ zip deduced [0..]
  let ans2 = product [ fromIntegral (ticketVals !! idx) | idx <- idxs]

  putStrLn $ "Part 2: " ++ show ans2