module Main where

import Text.Regex.TDFA
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
        valid_ecl = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

        byr_match = byr =~ "^[0-9]{4}$":: Bool
        byr_val = read byr :: Int

        iyr_match = iyr =~ "^[0-9]{4}$":: Bool
        iyr_val = read iyr :: Int

        eyr_match = eyr =~ "^[0-9]{4}$":: Bool
        eyr_val = read eyr :: Int

        hgt_match = hgt =~ "^[0-9]+(in|cm)$" :: Bool
        (_, hgt_val', hgt_unit) = hgt =~ "[0-9]+" :: (String, String, String)
        hgt_val = read hgt_val' :: Int
 
        hcl_match = hcl =~ "^#[0-9a-f]{6}$" :: Bool
        pid_match = pid =~ "^[0-9]{9}$" :: Bool

        criterion = 
          [ byr_match && iyr_match && eyr_match && hgt_match && hcl_match && pid_match
          , 1920 <= byr_val && byr_val <= 2002
          , 2010 <= iyr_val && iyr_val <= 2020
          , 2020 <= eyr_val && eyr_val <= 2030
          , if hgt_unit == "cm" then 150 <= hgt_val && hgt_val <= 193 else 59 <= hgt_val && hgt_val <= 76 
          , ecl `elem` valid_ecl]

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let passports = map (getPassport . unwords) $ splitOn [""] $ lines raw

  putStrLn "Part 1: "
  let valid1 = filter (\p -> validate1 p == 1) passports
  print $ length valid1

  putStrLn "Part 2: "
  print $ sum $ map validate2 valid1