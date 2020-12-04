module Main where

import Data.List.Split
import qualified Data.Map as Map

toField :: String -> (String, String)
toField field = (k, v)
  where [k, v] = splitOn ":" field

toFields :: String -> Map.Map String String
toFields line = Map.fromList $ map toField $ words line

validate :: String -> Int
validate passport = fromEnum $ Map.size fields == 8 || (Map.size fields == 7 && not (Map.member "cid" fields))
  where fields = toFields passport

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let passports = map unwords $ splitOn [""] $ lines raw

  putStrLn "Part 1: "
  print $ sum $ map validate passports