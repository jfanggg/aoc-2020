module Main where

import           Control.Monad               (forM_, foldM_, when)
import qualified Data.Vector.Unboxed.Mutable as V

main :: IO ()
main = do
  let input = [0, 6, 1, 7, 2, 19, 20]
  let (h, [t]) = splitAt (length input - 1) input

  memory <- V.replicate 30000000 (-1 :: Int)
  forM_ (zip h [1..]) (uncurry $ V.write memory)

  let loop val t = do
      when (t == 2020)      $ putStrLn $ "Part 1: " ++ show val
      when (t == 30000000)  $ putStrLn $ "Part 2: " ++ show val

      l <- V.read memory val
      V.write memory val t
      return (if l == -1 then 0 else t - l)

  foldM_ loop t [length input..30000000]