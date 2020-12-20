module Main where

import qualified Data.Map as M
import Debug.Trace

data Status = A | I deriving (Show, Eq);
data Vec4 = Vec4 {
  getX :: Int,
  getY :: Int,
  getZ :: Int,
  getW :: Int
} deriving (Show, Eq, Ord);
type Grid = M.Map Vec4 Status

maxInt :: Int
maxInt = maxBound

minInt :: Int
minInt = minBound

parseStatus :: Char -> Status
parseStatus c = case c of 
  '#' -> A
  '.' -> I

deltas :: [Vec4]
deltas = [Vec4 x y z w |  x <- [-1..1], y <- [-1..1], z <- [-1..1], w <- [-1..1], 
                        not (x == 0 && y == 0 && z == 0 && w == 0)]

parseRow :: Int -> String -> Grid
parseRow r row = M.fromList points
  where points = zipWith (\c val -> (Vec4 r c 0 0, parseStatus val)) [0..] row

axisRange :: Grid -> (Vec4 -> Int) -> [Int]
axisRange grid getter = [lo - 1 .. hi + 1]
  where func vec _ (lo, hi) = let val = getter vec in (min lo val, max hi val)
        acc = (maxInt, minInt)
        (lo, hi) = M.foldrWithKey func acc grid

addVec4 :: Vec4 -> Vec4 -> Vec4
addVec4 a b = Vec4 (getX a + getX b) (getY a + getY b) (getZ a + getZ b) (getW a + getW b)

readAt :: Grid -> Vec4 -> Vec4 -> Status
readAt grid pos dir = status
  where newPos = addVec4 pos dir
        status = M.findWithDefault I newPos grid

stepPosition :: Grid -> Vec4 -> Status
stepPosition grid pos = status
  where me = readAt grid pos (Vec4 0 0 0 0)
        neighbors = map (readAt grid pos) deltas
        neighborsA = length $ filter (== A) neighbors

        status = case me of 
          A -> if neighborsA == 2 || neighborsA == 3 then A else I
          I -> if neighborsA == 3 then A else I

stepGrid :: Bool -> Grid -> Grid
stepGrid p2 old = new
  where [xRange, yRange, zRange, wRange] = axisRange old <$> [getX, getY, getZ, getW]
        ws = if p2 then wRange else [0]
        coords = [Vec4 x y z w | x <- xRange, y <- yRange, z <- zRange, w <- ws]

        new = M.fromList $ map (\c -> (c, stepPosition old c)) coords

countA :: Grid -> Int
countA = length . M.filter (== A)

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let grid0 = M.unions $ zipWith parseRow [0..] $ lines raw

  let ans1 = countA $ iterate (stepGrid False) grid0 !! 6
  putStrLn $ "Part 1: " ++ show ans1

  let ans2 = countA $ iterate (stepGrid True) grid0 !! 6
  putStrLn $ "Part 2: " ++ show ans2