module Main where

import Data.Char ( digitToInt )

eval :: String -> Int
eval = run []
  where run stack [] = head stack
        run stack (h:t) = case h of 
          '*' -> let (f:s:r) = stack in run (f*s:r) t
          '+' -> let (f:s:r) = stack in run (f+s:r) t
          _   -> run (digitToInt h:stack) t

shuntingYard :: Bool -> String -> String
shuntingYard = run [] []
  where run output opStack _ [] = reverse output ++ opStack
        run output opStack p2 (h:t) = case h of 
          '+' -> let ops = takeWhile (`elem` if p2 then ['+'] else ['+', '*']) opStack in 
                  run (reverse ops ++ output) (h:drop (length ops) opStack) p2 t
          '*' -> let ops = takeWhile (`elem` ['+', '*']) opStack in 
                  run (reverse ops ++ output) (h:drop (length ops) opStack) p2 t
          '(' -> run output (h:opStack) p2 t
          ')' -> let ops = takeWhile (/= '(') opStack in
                  run (reverse ops ++ output) (drop (length ops + 1) opStack) p2 t
          _   -> run (h:output) opStack p2 t

solve :: Bool -> [String] -> Int
solve p2 exprs = sum $ map (eval . shuntingYard p2) exprs

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let exprs =  map (filter (/= ' ')) $ lines raw 
  
  let [ans1, ans2] = solve <$> [False, True] <*> pure exprs
  putStrLn $ "Part 1: " ++ show ans1
  putStrLn $ "Part 2: " ++ show ans2