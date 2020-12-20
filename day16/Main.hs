module Main where

import Text.Regex.PCRE
import Data.List ( isPrefixOf, transpose )
import Data.List.Split ( splitOn )
import Data.Maybe ( isNothing, mapMaybe )

type Range = (Int, Int)
data Rule = Rule {
  name :: String,
  ranges :: [Range]
} deriving (Show, Eq);

parseRule :: String -> Rule
parseRule line = Rule name [(a, b), (c, d)]
  where (_:name:vals):_ = line =~ "([a-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)" :: [[String]]
        [a, b, c, d] = map read vals :: [Int]

check :: Rule -> Int -> Bool
check (Rule _ rs) val = any (\ (lo, hi) -> lo <= val && val <= hi) rs

checkTicket :: [Rule] -> [Int] -> Maybe Int
checkTicket rules ticket = if null badVals then Nothing else Just $ sum badVals
  where isBad v = all (\rule -> not $ check rule v) rules
        badVals = filter isBad ticket

matrixify :: [String] -> [[Int]]
matrixify = map (map read . splitOn ",")

filterRules :: [Rule] -> [Int] -> [Rule]
filterRules rules group = filter (\rule -> all (check rule) group) rules

deduceRules :: [[Rule]] -> [Rule]
deduceRules old = if old == new then concat new else deduceRules new
  where confirmedNames = map (name . head) $ filter (\g -> length g == 1) old
        filterRuleGroup g 
          | length g > 1 = filter (\(Rule n _) -> n `notElem` confirmedNames) g
          | otherwise    = g

        new = map filterRuleGroup old

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let [rules', _:myTicket', _:tickets'] = splitOn [""] $ lines raw

  let rules     = map parseRule rules'
  let myTicket  = concat $ matrixify myTicket'
  let tickets   = matrixify tickets'

  let ans1 = sum $ mapMaybe (checkTicket rules) tickets
  putStrLn $ "Part 1: " ++ show ans1
  
  let goodTickets = filter (isNothing . checkTicket rules) tickets
  let valueGroups = transpose goodTickets

  let possibleRules = map (filterRules rules) valueGroups
  let assignedRules = zip (deduceRules possibleRules) myTicket
  let departures = filter (\(Rule n _, _) -> "departure" `isPrefixOf ` n) assignedRules

  let ans2 = product $ map snd departures
  putStrLn $ "Part 2: " ++ show ans2