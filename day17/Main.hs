module Main where

import qualified Data.Map as M
import Debug.Trace

data Status = A | I deriving (Show, Eq);
data Vec3 = Vec3 {
  getX :: Int,
  getY :: Int,
  getZ :: Int
} deriving (Show, Eq, Ord);
type Grid = M.Map Vec3 Status

maxInt :: Int
maxInt = maxBound

minInt :: Int
minInt = minBound

parseStatus :: Char -> Status
parseStatus c = case c of 
  '#' -> A
  '.' -> I

deltas :: [Vec3]
deltas = [Vec3 x y z | x <- [-1..1], y <- [-1..1], z <- [-1..1], not (x == 0 && y == 0 && z == 0)]

parseRow :: Int -> String -> Grid
parseRow r row = M.fromList points
  where points = zipWith (\c val -> (Vec3 r c 0, parseStatus val)) [0..] row

axisRange :: Grid -> (Vec3 -> Int) -> [Int]
axisRange grid getter = [lo - 1 .. hi + 1]
  where func vec _ (lo, hi) = let val = getter vec in (min lo val, max hi val)
        acc = (maxInt, minInt)
        (lo, hi) = M.foldrWithKey func acc grid

readAt :: Grid -> Vec3 -> Vec3 -> Status
readAt grid pos dir = status
  where newPos = Vec3 (getX pos + getX dir) (getY pos + getY dir) (getZ pos + getZ dir)
        status = M.findWithDefault I newPos grid

stepPosition :: Grid -> Vec3 -> Status
stepPosition grid pos = status
  where me = readAt grid pos (Vec3 0 0 0)
        neighbors = map (readAt grid pos) deltas
        neighborsA = length $ filter (== A) neighbors

        status = case me of 
          A -> if neighborsA == 2 || neighborsA == 3 then A else I
          I -> if neighborsA == 3 then A else I

stepGrid :: Grid -> Grid
stepGrid old = new
  where [xRange, yRange, zRange] = axisRange old <$> [getX, getY, getZ]
        coords = [Vec3 x y z | x <- xRange, y <- yRange, z <- zRange]

        new = M.fromList $ map (\c -> (c, stepPosition old c)) coords

countA :: Grid -> Int
countA = length . M.filter (== A)

main :: IO ()
main = do
  raw <- readFile "input.txt"

  let grid0 = M.unions $ zipWith parseRow [0..] $ lines raw
  let ans1 = countA $ iterate stepGrid grid0 !! 6
  putStrLn $ "Part 1: " ++ show ans1