module Main where

import qualified Data.Set as S
import Data.List.Split
import Data.Maybe 
import Data.Either

data State = State {
  pc :: Int,
  acc :: Int
} deriving Show

data Operation = Nop | Acc | Jmp deriving (Show, Eq)

type Instruction = (Operation, Int)
type Program = [Instruction]

parseOperation :: String -> Operation
parseOperation "nop" = Nop
parseOperation "acc" = Acc
parseOperation "jmp" = Jmp

processLine :: String -> Instruction
processLine s = (parseOperation op', arg)
  where [op', arg'] = splitOn " " s        

        sign = if head arg' == '+' then 1 else -1
        arg = sign * (read $ tail arg' :: Int)

stepState :: State -> Instruction -> State
stepState state (Nop, _)   = State (pc state + 1)   (acc state)
stepState state (Jmp, val) = State (pc state + val) (acc state)
stepState state (Acc, val) = State (pc state + 1)   (acc state + val)

run :: Program -> State -> S.Set Int -> Either Int Int
run program state seen = if pc state >= length program then Right (acc state) else recurse
  where nextState = stepState state (program !! pc state)
        nextPc = pc nextState 
        recurse = if S.member nextPc seen then Left (acc state) else run program nextState (S.union seen (S.singleton $ pc state))

alterProgram :: Program -> Int -> Maybe Program
alterProgram program index = altered
  where (before,(op, val):after) = splitAt index program
        altered
          | op == Jmp = Just (before ++ (Nop, val) : after)
          | op == Nop = Just (before ++ (Jmp, val) : after)
          | otherwise = Nothing

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let program = map processLine $ lines raw

  let state0 = State 0 0

  putStr "Part 1: "
  let Left ans1 = run program state0 S.empty
  print ans1

  putStr "Part 2: "
  let altered = mapMaybe (alterProgram program) [1..length program - 1]
  let ans2 = head $ rights $ map (\p -> run p state0 S.empty) altered

  print ans2