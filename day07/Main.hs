module Main where

import Text.Regex.PCRE
import qualified Data.Set as S
import qualified Data.Map as M

type Node = String
data Edge = Edge {
  parent :: Node,
  child :: Node,
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

getParentMap :: [Edge] -> M.Map Node (S.Set Node)
getParentMap edges = M.fromListWith S.union list
  where list = map (\e -> (child e, S.singleton (parent e))) edges

getChildrenMap :: [Edge] -> M.Map Node [(Node, Int)]
getChildrenMap edges = M.fromListWith (++) list
  where list = map (\e -> (parent e, [(child e, val e)])) edges

getAncestors ::  M.Map Node (S.Set Node) -> Node -> S.Set Node
getAncestors parentMap root = S.foldr S.union (S.singleton root) parentAncestors
  where parents = M.findWithDefault S.empty root parentMap
        parentAncestors = S.map (getAncestors parentMap) parents

countDescendants :: M.Map Node [(Node, Int)] -> String -> Int
countDescendants childrenMap root = 1 + sum childrenCounts
  where children = M.findWithDefault [] root childrenMap
        childrenCounts = map (\ (c, v) -> v * countDescendants childrenMap c) children

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let edges = concatMap processLine $ lines raw

  let parentMap = getParentMap edges
  let childrenMap = getChildrenMap edges

  putStr "Part 1: "
  print $ S.size (getAncestors parentMap "shiny gold") - 1

  putStr "Part 2: "
  print $ countDescendants childrenMap "shiny gold" - 1