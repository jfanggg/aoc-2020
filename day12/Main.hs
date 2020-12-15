module Main where

type Instruction = (Char, Int)
type Vec2 = (Int, Int)

data State = State {
  position :: Vec2,
  direction :: Vec2
} deriving (Show, Eq);

parseInstruction :: String -> Instruction
parseInstruction (h:t) = (h, val)
  where val = read t :: Int

toVec :: Char -> Vec2
toVec c = case c of 
  'N' -> ( 0,  1)
  'E' -> ( 1,  0)
  'S' -> ( 0, -1)
  'W' -> (-1,  0)

ccw :: Vec2 -> Vec2
ccw (x, y) = (-y, x)

rotate :: Vec2 -> Int -> Vec2
rotate vec degs = iterate ccw vec !! turns
  where turns = (degs `div` 90) `mod` 4

step :: Bool -> State -> Instruction -> State
step p2 (State pos@(px, py) dir@(dx, dy)) (c, v) = case c of 
  'L' ->  State pos (rotate dir v)
  'R' ->  State pos (rotate dir (-v))
  'F' ->  State (px + v * dx, py + v * dy) dir
  _   ->  let (dX, dY) = toVec c in 
          if p2 then State pos (dx + v * dX, dy + v * dY) 
                else State (px + v * dX, py + v * dY) dir

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let instructions = map parseInstruction $ lines raw
  let east = toVec 'E'

  let init1 = State (0, 0) east
  let State (x1, y1) _ = foldl (step False) init1 instructions 
  putStr "Part 1: "
  print $ abs x1 + abs y1

  let init2 = State (0, 0) (10, 1)
  let State (x2, y2) _ = foldl (step True) init2 instructions 
  putStr "Part 2: "
  print $ abs x2 + abs y2