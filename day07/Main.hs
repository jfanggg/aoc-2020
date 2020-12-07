module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Text.Regex.PCRE

type Requirement = (String, Int)
type Edge = (String, String, Int)

processReq :: String -> Requirement
processReq s = (color, amount)
  where (head:tail) = words s
        amount = read head :: Int 
        color = unwords tail

processLine :: String -> [Edge]
processLine line = map (\(child, num) -> (parent, child, num)) reqs 
  where c = line =~ "^(.*?) bags" :: [[String]]
        [_, parent] = head c

        r = line =~ "(\\d+ .*?) bag" :: [[String]]
        reqs = map (\[_, req] -> processReq req) r

getAncestors :: [Edge] -> String -> [String]
getAncestors allEdges root = root : concat parentAncestors
  where edges = filter (\(_, c, _) -> c == root) allEdges
        parents = map (\(p, _, _) -> p) edges
        parentAncestors = map (getAncestors allEdges) parents

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let input = lines raw

  let edges = concatMap processLine input
  let ancestors = S.fromList $ getAncestors edges "shiny gold"

  putStr "Part 1: "
  print $ S.size ancestors - 1