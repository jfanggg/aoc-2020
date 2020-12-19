module Main where

import qualified Data.Map as M

type Memory = M.Map Int Int
data State = State {
  val :: Integer,
  time :: Integer,
  memory :: M.Map Integer Integer
} deriving (Eq, Show);

step :: State -> State
step (State v t memory) = State v' (t + 1) (M.insert v t memory)
  where v' = if M.member v memory then t - memory M.! v else 0

main :: IO ()
main = do
  let input = [0, 6, 1, 7, 2, 19, 20]
  let (h, [t]) = splitAt (length input - 1) input

  let memory = M.fromList $ zip h [1..]

  let state0 = State t (fromIntegral $ length input) memory
  let seq = iterate step state0

  let State ans1 _ _ = seq !! (2020 - length input)
  putStrLn $ "Part 1: " ++ show ans1