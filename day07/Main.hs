module Main where

import Text.Regex.PCRE
import qualified Data.Set as S

data Edge = Edge {
  parent :: String,
  child :: String,
  val :: Int
}

-- e.g. "1 bright white" -> ("bright white", 1)
processReq :: String -> (String, Int)
processReq s = (color, amount)
  where (head:tail) = words s
        amount = read head :: Int 
        color = unwords tail

processLine :: String -> [Edge]
processLine line = edges
  where bagCapture = line =~ "^(.*?) bags" :: [[String]]
        [_, parent] = head bagCapture

        reqsCapture = line =~ "(\\d+ .*?) bag" :: [[String]]
        reqs = map (\[_, req] -> processReq req) reqsCapture

        edges = map (uncurry $ Edge parent) reqs 

getAncestors :: [Edge] -> String -> S.Set String
getAncestors allEdges root = foldr S.union (S.singleton root) parentAncestors
  where edges = filter (\e -> child e == root) allEdges
        parentAncestors = map (getAncestors allEdges . parent) edges

countDescendants :: [Edge] -> String -> Int
countDescendants allEdges root = 1 + sum childrenCounts
  where edges = filter (\e -> parent e == root) allEdges
        childrenCounts = map (\e -> val e * countDescendants allEdges (child e)) edges

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let edges = concatMap processLine $ lines raw

  putStr "Part 1: "
  print $ S.size (getAncestors edges "shiny gold") - 1

  putStr "Part 2: "
  print $ countDescendants edges "shiny gold" - 1