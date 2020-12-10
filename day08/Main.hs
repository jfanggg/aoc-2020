module Main where

import qualified Data.Set as S
import qualified Data.Vector as V
import Data.List.Split ( splitOn )
import Data.Maybe ( mapMaybe ) 
import Data.Either ( rights )

data State = State {
  pc :: Int,
  acc :: Int
} deriving Show

data Operation = Nop | Acc | Jmp deriving (Show, Eq)

type Instruction = (Operation, Int)
type Program = V.Vector Instruction

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

run' :: State -> S.Set Int -> Program -> Either Int Int
run' state seen program = res 
  where terminated = pc state >= length program

        nextState = stepState state (program V.! pc state)
        nextSeen = S.union seen (S.singleton $ pc state)
        willLoop = S.member (pc nextState) seen

        res
          | terminated  = Right (acc state) 
          | willLoop    = Left  (acc state) 
          | otherwise   = run' nextState nextSeen program

run :: Program -> Either Int Int
run = run' (State 0 0) S.empty 

alterProgram :: Program -> Int -> Maybe Program
alterProgram program index = altered
  where (op, val) = program V.! index
        altered
          | op == Jmp = Just $ program V.// [(index, (Nop, val))]
          | op == Nop = Just $ program V.// [(index, (Jmp, val))]
          | otherwise = Nothing

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let program = V.fromList $ map processLine $ lines raw

  let Left ans1 = run program
  putStr "Part 1: "
  print ans1

  let altered = mapMaybe (alterProgram program) [0..length program - 1]
  let ans2 = head $ rights $ map run altered
  putStr "Part 2: "
  print ans2