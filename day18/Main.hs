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
shuntingYard :: Bool -> [Char] -> [Char] -> String -> String
shuntingYard p2 output opStack [] = reverse output ++ opStack
shuntingYard p2 output opStack (h:t) 
  | h `elem` ['0'..'9'] = shuntingYard p2 (h:output) opStack t
  | h == '+'            = let ops = takeWhile (`elem` if p2 then ['+'] else ['+', '*']) opStack in 
                          shuntingYard p2 (reverse ops ++ output) (h:drop (length ops) opStack)t
  | h == '*'            = let ops = takeWhile (`elem` ['+', '*']) opStack in 
                          shuntingYard p2 (reverse ops ++ output) (h:drop (length ops) opStack)t
  | h == '('            = shuntingYard p2 output (h:opStack) t
  | h == ')'            = let ops = takeWhile (/= '(') opStack in
                          shuntingYard p2 (reverse ops ++ output) (drop (length ops + 1) opStack) t

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let exprs =  map (filter (/= ' ')) $ lines raw 
  
  let ans1 = sum $ map (eval [] . shuntingYard False [] []) exprs
  putStrLn $ "Part 1: " ++ show ans1

  let ans2 = sum $ map (eval [] . shuntingYard True [] []) exprs
  putStrLn $ "Part 2: " ++ show ans2