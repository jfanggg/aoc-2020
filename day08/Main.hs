module Main where

import qualified Data.Set as S
import Data.List.Split
import Data.Maybe 

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

findLoop :: Program -> State -> S.Set Int -> Maybe Int
findLoop program state seen = if pc state >= length program then Nothing else recurse
  where nextState = stepState state (program !! pc state)
        nextPc = pc nextState 
        recurse = if S.member nextPc seen then Just $ acc state else findLoop program nextState (S.union seen (S.singleton $ pc state))

runProgram :: Program -> State -> Int
runProgram program state = if pc state >= length program then acc state else recurse
  where nextState = stepState state (program !! pc state)
        recurse = runProgram program nextState

alterProgram :: Program -> Int -> Maybe Program
alterProgram program line = new 
  where (before,old:after) = splitAt line program
        (op, val) = old
        new
          | op == Jmp = Just (before ++ (Nop, val) : after)
          | op == Nop = Just (before ++ (Jmp, val) : after)
          | otherwise = Nothing

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let program = map processLine $ lines raw

  putStr "Part 1: "
  print $ findLoop program (State 0 0) S.empty

  putStr "Part 2: "
  let altered = mapMaybe (alterProgram program) [1..length program - 1]
  let correct = head $ filter (\p -> Data.Maybe.isNothing (findLoop p (State 0 0) S.empty)) altered

  print $ runProgram correct (State 0 0)