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

parseRuleCapture :: [String] -> Maybe Rule
parseRuleCapture xs = Just $ Rule name [(a, b), (c, d)]
  where _:name:vals = xs
        [a, b, c, d] = map read vals :: [Int]

parseRule :: String -> Maybe Rule
parseRule line = rule
  where capture = line =~ "([a-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)" :: [[String]]
        rule = if null capture then Nothing else parseRuleCapture (head capture)

check :: Rule -> Int -> Bool
check (Rule _ rs) val = any (\ (lo, hi) -> lo <= val && val <= hi) rs

checkRow :: [Rule] -> [Int] -> Maybe Int
checkRow rules row = if null invalid then Nothing else Just $ sum invalid
  where invalid = filter (\v -> not $ any (`check` v) rules) row

matrixify :: [String] -> [[Int]]
matrixify = map (map read . splitOn ",")

validateRules :: [Rule] -> [Int] -> [Rule]
validateRules rules group = filter (\r -> all (check r) group) rules

deduceRules :: [[Rule]] -> [[Rule]]
deduceRules old = if old == new then new else deduceRules new
  where confirmedNames = map (name . head) $ filter (\g -> length g == 1) old
        filterRuleGroup g 
          | length g > 1 = filter (\(Rule n _) -> n `notElem` confirmedNames) g
          | otherwise    = g

        new = map filterRuleGroup old

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let [rules', _:myTicket', _:tickets'] = splitOn [""] $ lines raw

  let rules     = mapMaybe parseRule rules'
  let myTicket  = head (matrixify myTicket')
  let tickets   = matrixify tickets'

  let ans1 = sum $ mapMaybe (checkRow rules) tickets
  putStrLn $ "Part 1: " ++ show ans1
  
  let validTickets = filter (isNothing . checkRow rules) tickets
  let valueGroups = transpose validTickets

  let possibleRules = map (validateRules rules) valueGroups
  let assignments = zip myTicket (concat $ deduceRules possibleRules)
  let departures = filter ( \(_, Rule n _) -> "departure" `isPrefixOf ` n) assignments

  let ans2 = product $ map fst departures
  putStrLn $ "Part 2: " ++ show ans2