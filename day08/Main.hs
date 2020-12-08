module Main where

import qualified Data.Set as S
import Data.List.Split ( splitOn )
import Data.Maybe ( mapMaybe ) 
import Data.Either ( rights )

data State = State {
  pc :: Int,
  acc :: Int
} deriving Show

data Operation = Nop | Acc | Jmp deriving (Show, Eq)

type Instruction = (Operation, Int)
type Program = [Instruction]

parseOperation :: String -> Operation
parseOperation s = case s of
  "nop" -> Nop
  "acc" -> Acc
  "jmp" -> Jmp

processLine :: String -> Instruction
processLine s = (parseOperation op, arg)
  where [op, sign:num] = splitOn " " s
        arg = (if sign == '+' then 1 else -1) * (read num :: Int)

stepState :: State -> Instruction -> State
stepState state instruction = case instruction of 
  (Nop, _)    -> State (pc state + 1)   (acc state)
  (Jmp, val)  -> State (pc state + val) (acc state)
  (Acc, val)  -> State (pc state + 1)   (acc state + val)

run :: State -> S.Set Int -> Program -> Either Int Int
run state seen program = res 
  where terminated = pc state >= length program

        nextState = stepState state (program !! pc state)
        nextSeen = S.union seen (S.singleton $ pc state)
        willLoop = S.member (pc nextState) seen

        res
          | terminated  = Right (acc state) 
          | willLoop    = Left  (acc state) 
          | otherwise   = run nextState nextSeen program

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
  let run0 = run state0 S.empty

  putStr "Part 1: "
  let Left ans1 = run0 program
  print ans1

  putStr "Part 2: "
  let altered = mapMaybe (alterProgram program) [1..length program - 1]
  let ans2 = head $ rights $ map run0 altered

  print ans2