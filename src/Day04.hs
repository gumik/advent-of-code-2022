module Day04 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)

solution :: Solution Int Int
solution = Solution "day04" "" run

run :: String -> (Int, Int)
run input = let assignments = parse input in (part1 assignments, part2 assignments)

type Pair = (Int, Int)
type Assignment = (Pair, Pair)

parse :: String -> [Assignment]
parse = map parseLine . lines

parseLine :: String -> Assignment
parseLine l = let 
    [p1, p2] = splitOn "," l
    in (parsePair p1, parsePair p2)

parsePair :: String -> Pair
parsePair s = let [a, b] = splitOn "-" s in (readNum a, readNum b)


part1 :: [Assignment] -> Int
part1 = length . filter (==True) . map fullyContains

fullyContains :: Assignment -> Bool
fullyContains ((a1, a2), (b1, b2)) = (b1 >= a1 && b2 <= a2) || (a1 >= b1 && a2 <= b2)

part2 :: [Assignment] -> Int
part2 = length . filter (==True) . map overlap

overlap :: Assignment -> Bool
overlap ((a1, a2), (b1, b2)) = a2 >= b1 && a1 <= b2
