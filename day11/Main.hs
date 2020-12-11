module Main where

import Data.List
import qualified Data.Vector as V
import Data.Maybe ( mapMaybe )
import Debug.Trace

data Status = F | O | E deriving (Show, Eq);
type Grid = V.Vector (V.Vector Status)
type Vec2 = (Int, Int)

processChar :: Char -> Status
processChar c = case c of 
  'L' -> E
  '.' -> F
  '#' -> O

deltas :: [Vec2]
deltas = [(r, c) | r <- [-1..1], c <- [-1..1], not (r == 0 && c == 0)]

look' :: Bool -> Grid -> Vec2 -> Vec2 -> Maybe Status
look' extend grid (r, c) (dR, dC) = res
  where val = grid V.!? (r + dR) >>= (V.!? (c + dC))

        res 
          | extend && val == Just F = look' extend grid (r + dR, c + dC) (dR, dC)
          | otherwise               = val

look :: Grid -> Vec2 -> Vec2 -> Maybe Status
look = look' False

lookExtend :: Grid -> Vec2 -> Vec2 -> Maybe Status
lookExtend = look' True

updatePosition :: Bool -> Grid -> (Int, Int) -> Status
updatePosition p2 grid pos = updated
  where threshold = if p2 then 5 else 4
        lookFunc = if p2 then lookExtend else look

        Just myStatus = look grid pos (0, 0)
        surroundings = mapMaybe (lookFunc grid pos) deltas

        updated = case myStatus of 
          E -> if O `notElem` surroundings then O else E
          O -> if length (filter (== O) surroundings) >= threshold then E else O
          F -> F

step :: Bool -> Grid -> Grid
step p2 grid = V.fromList $ map mapR [0..num_r - 1]
  where num_r = V.length grid
        num_c = V.length (grid V.! 0)

        update = curry $ updatePosition p2 grid 
        mapR = \r -> V.fromList $ map (update r) [0..num_c - 1]

findEquilibrium :: Bool -> Grid -> Grid
findEquilibrium p2 old = if old /= new then findEquilibrium p2 new else old
  where new = step p2 old

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let grid = V.fromList $ map (V.fromList . map processChar) $ lines raw
  let countOccupancies = sum . V.map (length . V.filter (==O))

  let ans1 = countOccupancies $ findEquilibrium False grid
  putStr "Part 1: "
  print ans1 

  let ans2 = countOccupancies $ findEquilibrium True grid
  putStr "Part 2: "
  print ans2