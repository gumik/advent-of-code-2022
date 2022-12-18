module Day16 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

solution = Solution "day16" "" run

run input = let
    valves = parse input
    in {- traceShow valves -} (NoSolution, NoSolution)

data Valve = Valve String Int [String] deriving (Show)

parse :: String -> [Valve]
parse = map parseLine . filter (/="") . lines

parseLine :: String -> Valve
parseLine str = let
    w = words str
    rate = readNum $ filter (/=';') $ last $ splitOn "=" $ w !! 4
    tunnels = map (filter (/= ',')) $ drop 9 w
    in Valve (w !! 1) rate tunnels

{-
Try:
* Find distances between each pairs of nodes with non-zero flow rate.
  * Calculate BFS for each such node. There is not much such nodes - 14 in input data.
* Then do brute force.
  * Start from AA and check all other possibility - all nodes with non-zero flow rate.
  * Hopefully the path will not contain so much nodes becaues of long distances.
  * Possible improvement is to memorize intermediate steps, eg.
    1 - 2 - 3 - 4
    a. go to 1, then 2
    b. go to 2, then 1
    in both cases nodes 3 and 4 left. The result for them should be the same. The only difference is different starting point.
-}