module Main where

import qualified Data.Vector as V
import Data.Maybe ( mapMaybe )

data Status = E | O | F deriving (Show, Eq);
type Grid = V.Vector (V.Vector Status)
type Vec2 = (Int, Int)

range :: Int -> V.Vector Int
range n = V.generate n id

parseStatus :: Char -> Status
parseStatus c = case c of 
  'L' -> E
  '#' -> O
  '.' -> F

deltas :: [Vec2]
deltas = [(r, c) | r <- [-1..1], c <- [-1..1], not (r == 0 && c == 0)]

look :: Bool -> Grid -> Vec2 -> Vec2 -> Maybe Status
look extend grid (r, c) (dR, dC) = res
  where val = grid V.!? (r + dR) >>= (V.!? (c + dC))

        res 
          | extend && val == Just F = look extend grid (r + dR, c + dC) (dR, dC)
          | otherwise               = val

stepPosition :: Bool -> Grid -> Vec2 -> Status
stepPosition p2 grid pos = res
  where threshold = if p2 then 5 else 4

        Just status = look False grid pos (0, 0)
        surroundings = mapMaybe (look p2 grid pos) deltas

        res = case status of 
          E -> if O `notElem` surroundings then O else E
          O -> if length (filter (== O) surroundings) >= threshold then E else O
          F -> F

stepGrid :: Bool -> Grid -> Grid
stepGrid p2 grid = V.map (\r -> V.map (\c -> update (r, c)) cols) rows
  where rows = range $ V.length grid
        cols = range $ V.length (grid V.! 0)

        update = stepPosition p2 grid

finalOccupied :: Bool -> Grid -> Int
finalOccupied p2 grid = sum $ V.map (length . V.filter (==O)) fixedPoint
  where run old = let new = stepGrid p2 old in 
                  if old /= new then run new else old

        fixedPoint = run grid

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let grid = V.fromList $ map (V.fromList . map parseStatus) $ lines raw
  let [ans1, ans2] = flip finalOccupied grid <$> [False, True]

  putStr "Part 1: "
  print ans1 

  putStr "Part 2: "
  print ans2