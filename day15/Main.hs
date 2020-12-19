module Main where

import           Control.Monad               (forM_, foldM_, when)
import qualified Data.Vector.Unboxed.Mutable as V

n :: Int
n = 30000000

main :: IO ()
main = do
  let input = [0, 6, 1, 7, 2, 19, 20]
  let (start, [v]) = splitAt (length input - 1) input

  memory <- V.replicate n (-1 :: Int)
  forM_ (zip start [1..]) (uncurry $ V.write memory)

  let loop val time = do
      when (time == 2020) $ putStrLn $ "Part 1: " ++ show val
      when (time == n)    $ putStrLn $ "Part 2: " ++ show val

      lookup <- V.read memory val
      V.write memory val time
      return (if lookup == -1 then 0 else time - lookup)

  foldM_ loop v [length input..n]