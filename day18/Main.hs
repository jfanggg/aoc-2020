module Main where

import Debug.Trace
import Data.Char ( digitToInt )

eval :: [Int] -> String -> Int
-- eval stack tokens | trace ("eval " ++ show stack ++ " " ++ show tokens) False = undefined
eval stack [] = head stack
eval stack (h:t)
  | h `elem` ['0'..'9'] = eval (digitToInt h:stack) t
  | h == '*'            = let (f:s:r) = stack in eval (f*s:r) t
  | h == '+'            = let (f:s:r) = stack in eval (f+s:r) t

-- shuntingYard output operator tokens | trace ("shuntingYard " ++ show tokens ++ " " ++ show output ++ " " ++ show operator) False = undefined
shuntingYard :: [Char] -> [Char] -> String -> String
shuntingYard output opStack [] = reverse output ++ opStack
shuntingYard output opStack (h:t) 
  | h `elem` ['0'..'9'] = shuntingYard (h:output) opStack t
  | h `elem` ['+', '*'] = let ops = takeWhile (`elem` ['+', '*']) opStack in 
                          shuntingYard (reverse ops ++ output) (h:drop (length ops) opStack)t
  | h == '('            = shuntingYard output (h:opStack) t
  | h == ')'            = let ops = takeWhile (/= '(') opStack in
                          shuntingYard (reverse ops ++ output) (drop (length ops + 1) opStack) t

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let exprs =  map (filter (/= ' ')) $ lines raw 
  
  let ans1 = sum $ map (eval [] . shuntingYard [] []) exprs
  putStrLn $ "Part 1: " ++ show ans1

  print ""