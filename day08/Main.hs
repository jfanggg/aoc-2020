module Main where

import Text.Regex.PCRE
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.Split

data State = State {
  pc :: Int,
  acc :: Int
} deriving Show
data Operation = Nop | Acc | Jmp deriving Show

type Instruction = (Operation, Int)
type Program = [Instruction]

parseOperation :: String -> Operation
parseOperation "nop" = Nop
parseOperation "acc" = Acc
parseOperation "jmp" = Jmp

sign :: Char -> Int
sign '+' =  1
sign '-' = -1

processLine :: String -> Instruction
processLine s = (parseOperation op', arg)
  where [op', arg'] = splitOn " " s
        arg = sign (head arg') * (read $ tail arg' :: Int)

stepState :: State -> Instruction -> State
stepState state (Nop, _) = State (pc state + 1) (acc state)
stepState state (Jmp, val) = State (pc state + val) (acc state)
stepState state (Acc, val) = State (pc state + 1) (acc state + val)

findLoop :: Program -> State -> S.Set Int -> Int
findLoop program state seen = if S.member nextPc seen then acc state else findLoop program nextState (S.union seen (S.singleton $ pc state))
  where nextState = stepState state (program !! pc state)
        nextPc = pc nextState 

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let program = map processLine $ lines raw

  putStr "Part 1: "
  print $ findLoop program (State 0 0) S.empty