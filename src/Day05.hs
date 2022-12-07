module Day05 where

import Common (Solution(Solution), NoSolution(..), readNum, readNum', setAt)
import Data.List.Split (splitOn)
import Data.List (transpose)

solution :: Solution String String
solution = Solution "day05" "" run

run :: String -> (String, String)
run input = let
    (stacks, rearrangements) = parse input
    in (part1 stacks rearrangements, part2 stacks rearrangements)

type Count = Int
type Position = Int
data Rearrangement = Move Count Position Position deriving (Show)
type Stacks = [[Char]]

parse :: String -> (Stacks, [Rearrangement])
parse input = let [stacksStr, rearrangementsStr] = splitOn "\n\n" input
    in (parseStacks stacksStr, parseRearrangements rearrangementsStr)

parseStacks :: String -> Stacks
parseStacks = map (filter (/= ' ')) . transpose . map readCrateLine . init . lines

readCrateLine :: String -> [Char]
readCrateLine = map snd . filter ((== 1) . (`mod` 4) . fst) . zip [0..]

parseRearrangements :: String -> [Rearrangement]
parseRearrangements = map parseRearrangement . lines

parseRearrangement :: String -> Rearrangement
parseRearrangement line = let
    [_, countStr, _, fromStr, _, toStr] = words line
    in Move (readNum countStr) (readNum fromStr - 1) (readNum toStr - 1)


part1 :: Stacks -> [Rearrangement] -> String
part1 = solve rearrange

part2 :: Stacks -> [Rearrangement] -> String
part2 = solve rearrange2

solve :: (Stacks -> Rearrangement -> Stacks) -> Stacks -> [Rearrangement] -> String
solve rearrange stacks rearrangements = map head $ foldl rearrange stacks rearrangements

rearrange :: Stacks -> Rearrangement -> Stacks
rearrange stacks (Move 0 _ _) = stacks
rearrange stacks (Move count from to) = let
    fromStack = stacks !! from
    crate = head fromStack
    toStack = stacks !! to
    stacks' = setAt from (tail fromStack) $ setAt to (crate:toStack) stacks
    in rearrange stacks' (Move (count-1) from to)

rearrange2 :: Stacks -> Rearrangement -> Stacks
rearrange2 stacks (Move count from to) = let
    fromStack = stacks !! from
    crates = take count fromStack
    toStack = stacks !! to
    stacks' = setAt from (drop count fromStack) $ setAt to (crates ++ toStack) stacks
    in stacks'
