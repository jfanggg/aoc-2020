module Main where

import Text.Regex.PCRE
import Data.List.Split ( splitOn )
import qualified Data.Map as Map

type Passport = Map.Map String String

getField :: String -> (String, String)
getField field = (k, v)
  where [k, v] = splitOn ":" field

getPassport :: String -> Passport
getPassport line = Map.fromList $ map getField $ words line

validate1 :: Passport -> Int
validate1 p = fromEnum $ Map.size p == 8 || (Map.size p == 7 && not (Map.member "cid" p))

validate2 :: Passport -> Int
validate2 p = fromEnum $ and criterion
  where [byr, iyr, eyr, hgt, hcl, ecl, pid] = map (p Map.!) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        byrMatch = byr =~ "^[0-9]{4}$":: Bool
        byrValue = read byr :: Int

        iyrMatch = iyr =~ "^[0-9]{4}$":: Bool
        iyrValue = read iyr :: Int

        eyrMatch = eyr =~ "^[0-9]{4}$":: Bool
        eyrValue = read eyr :: Int

        hgtMatch = hgt =~ "^[0-9]+(in|cm)$" :: Bool
        (_, hgtValue', hgtUnit) = hgt =~ "[0-9]+" :: (String, String, String)
        hgtValue = read hgtValue' :: Int
 
        hclMatch = hcl =~ "^#[0-9a-f]{6}$" :: Bool
        pidMatch = pid =~ "^[0-9]{9}$" :: Bool

        criterion = 
          [ byrMatch && iyrMatch && eyrMatch && hgtMatch && hclMatch && pidMatch
          , 1920 <= byrValue && byrValue <= 2002
          , 2010 <= iyrValue && iyrValue <= 2020
          , 2020 <= eyrValue && eyrValue <= 2030
          , if hgtUnit == "cm" then 150 <= hgtValue && hgtValue <= 193 else 59 <= hgtValue && hgtValue <= 76 
          , ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] ]

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let passports = map (getPassport . unwords) $ splitOn [""] $ lines raw

  putStrLn "Part 1: "
  let valid1 = filter (\p -> validate1 p == 1) passports
  print $ length valid1

  putStrLn "Part 2: "
  print $ sum $ map validate2 valid1