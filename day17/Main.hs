module Main where

import qualified Data.Map as M

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
  where func vec _ (l, h) = let val = getter vec in (min l val, max h val)
        (lo, hi) = M.foldrWithKey func (maxInt, minInt) grid

addVec4 :: Vec4 -> Vec4 -> Vec4
addVec4 a b = Vec4 x y z w
  where apply f = f a + f b 
        [x, y, z, w] = apply <$> [getX, getY, getZ, getW]

stepPosition :: Grid -> Vec4 -> Status
stepPosition grid pos = status
  where readAt grid pos = M.findWithDefault I pos grid
    
        me = readAt grid pos
        neighbors = map (readAt grid . addVec4 pos) deltas
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

solve :: Bool -> Grid -> Int
solve p2 grid0 = active
  where grid6 = iterate (stepGrid p2) grid0 !! 6
        active = length $ M.filter (== A) grid6

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let grid0 = M.unions $ zipWith parseRow [0..] $ lines raw

  let [ans1, ans2] = solve <$> [False, True] <*> pure grid0
  putStrLn $ "Part 1: " ++ show ans1
  putStrLn $ "Part 2: " ++ show ans2