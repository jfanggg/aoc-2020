module Main where

import Text.Regex.PCRE
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import Debug.Trace
import Data.Maybe ( mapMaybe )

data Status = F | O | E deriving (Show, Eq);
type Grid = V.Vector (V.Vector Status)

-- myfun a b | trace ("myfun " ++ show a ++ " " ++ show b) False = undefined

processChar :: Char -> Status
processChar c = case c of 
  'L' -> E
  '.' -> F
  '#' -> O

deltas :: [(Int, Int)]
deltas = [(1,0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

readAt :: Grid -> (Int, Int) -> Maybe Status
readAt grid (r, c) = val
  where validR = 0 <= r && r < V.length grid
        validC = 0 <= c && c < V.length (grid V.! 0)

        val 
          | validR && validC  = Just $ (grid V.! r) V.! c 
          | otherwise         = Nothing

updatePosition :: Grid -> (Int, Int) -> Status
updatePosition grid (r, c) = updated
  where Just myStatus = readAt grid (r, c)
        surroundings = mapMaybe (\ (dR, dC) -> readAt grid (r + dR, c + dC)) deltas

        updated = case myStatus of 
          E -> if O `notElem` surroundings then O else E
          O -> if length (filter (== O) surroundings) >= 4 then E else O
          F -> F

readAtExtend :: Grid -> (Int, Int) -> (Int, Int) -> Maybe Status
readAtExtend grid (r, c) (dR, dC) = val
  where validR = 0 <= r && r < V.length grid
        validC = 0 <= c && c < V.length (grid V.! 0)

        val 
          | validR && validC  = if (grid V.! r) V.! c == F then readAtExtend grid (r + dR, c + dC) (dR, dC) else Just $ (grid V.! r) V.! c 
          | otherwise         = Nothing

updatePosition2 :: Grid -> (Int, Int) -> Status
updatePosition2 grid (r, c) = updated
  where Just myStatus = readAt grid (r, c)
        surroundings = mapMaybe (\ (dR, dC) -> readAtExtend grid (r + dR, c + dC) (dR, dC)) deltas

        updated = case myStatus of 
          E -> if O `notElem` surroundings then O else E
          O -> if length (filter (== O) surroundings) >= 5 then E else O
          F -> F

step :: Grid -> Grid
step grid = V.fromList $ map mapR [0..num_r - 1]
  where num_r = V.length grid
        num_c = V.length (grid V.! 0)

        update = curry $ updatePosition grid 
        mapR = \r -> V.fromList $ map (update r) [0..num_c - 1]

step2 :: Grid -> Grid
step2 grid = V.fromList $ map mapR [0..num_r - 1]
  where num_r = V.length grid
        num_c = V.length (grid V.! 0)

        update = curry $ updatePosition2 grid 
        mapR = \r -> V.fromList $ map (update r) [0..num_c - 1]

simulate :: Grid -> Grid
simulate old = if old /= new then simulate new else old
  where new = step old

simulate2 :: Grid -> Grid
simulate2 old = if old /= new then simulate2 new else old
  where new = step2 old

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let grid = V.fromList $ map (V.fromList . map processChar) $ lines raw

  let fixed = simulate grid
  let ans1 = sum $ V.map (length . V.filter (==O)) fixed
  putStr "Part 1: "
  print ans1 

  let fixed2 = simulate2 grid
  let ans2 = sum $ V.map (length . V.filter (==O)) fixed2
  putStr "Part 2: "
  print ans2

  print ""