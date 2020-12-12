module Main where

import qualified Data.Map as M

data Direction = N | E | S | W deriving (Show, Eq, Enum, Ord);
type Instruction = (Char, Int)
type Vec2 = (Int, Int)

data State = State {
  pos :: Vec2,
  dir :: Direction
} deriving (Show, Eq);

data State2 = State2 {
  pos2 :: Vec2,
  wp2 :: Vec2,
  dir2 :: Direction
} deriving (Show, Eq);

next :: Direction -> Direction 
next W = N
next d = succ d

prev :: Direction -> Direction 
prev N = W
prev d = pred d

toDirection :: Char -> Direction
toDirection c = case c of 
  'N' -> N
  'E' -> E
  'S' -> S
  'W' -> W

toVec :: Direction -> Vec2
toVec c = dirs M.! c
  where dirs = M.fromList [ (N, ( 0,  1)), (E, ( 1,  0)), (S, ( 0, -1)), (W, (-1,  0)) ]

updateVec :: Char -> Direction -> Direction
updateVec 'L' v = prev v
updateVec 'R' v = next v
updateVec _ v = v

parseInstruction :: String -> Instruction
parseInstruction (h:t) = (h, val)
  where val = read t :: Int

step :: State -> Instruction -> State
step state ('L', v) = State (pos state) d
                      where d = iterate prev (dir state) !! (v `div` 90)
step state ('R', v) = State (pos state) d
                      where d = iterate next (dir state) !! (v `div` 90)
step state ('F', v) = State (x + v * dX, y + v * dY) (dir state)
                      where (x, y) = pos state
                            (dX, dY) = toVec $ dir state 
step state (c, v)   = State (x + v * dX, y + v * dY) (dir state)
                      where (x, y) = pos state
                            (dX, dY) = toVec $ toDirection c
ccw :: Vec2 -> Vec2
ccw (p_x, p_y) = (-p_y, p_x)

cw :: Vec2 -> Vec2
cw (p_x, p_y) = (p_y, -p_x)

step2 :: State2 -> Instruction -> State2
step2 state ('L', v) =  State2 (pos2 state) newWp (dir2 state)
                        where newWp = iterate ccw (wp2 state) !! (v `div` 90)
step2 state ('R', v) =  State2 (pos2 state) newWp (dir2 state)
                        where newWp = iterate cw (wp2 state) !! (v `div` 90)
step2 state ('F', v) =  State2 (x + v * wX, y + v * wY) (wp2 state) (dir2 state)
                        where (x, y) = pos2 state
                              (wX, wY) = wp2 state
step2 state (c, v)   =  State2 (pos2 state) (x + v * dX, y + v * dY) (dir2 state)
                        where (x, y) = wp2 state
                              (dX, dY) = toVec $ toDirection c

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let instructions = map parseInstruction $ lines raw
  let init = State (0, 0) E

  let State (x, y) _ = foldl step init instructions
  let ans1 = abs x + abs y
  putStr "Part 1: "
  print ans1

  let init2 = State2 (0, 0) (10, 1) E
  let State2 (x2, y2) _ _ = foldl step2 init2 instructions

  let ans2 = abs x2 + abs y2
  putStr "Part 2: "
  print ans2