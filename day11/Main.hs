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

step :: Grid -> Grid
step grid = V.fromList $ map mapR [0..num_r - 1]
  where num_r = V.length grid
        num_c = V.length (grid V.! 0)

        update = curry $ updatePosition grid 
        mapR = \r -> V.fromList $ map (update r) [0..num_c - 1]

simulate :: Grid -> Grid
simulate old = if old /= new then simulate new else old
  where new = step old

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let grid = V.fromList $ map (V.fromList . map processChar) $ lines raw

  let fixed = simulate grid
  let ans1 = sum $ V.map (length . V.filter (==O)) fixed
  putStr "Part 1: "
  print ans1 

  print ""