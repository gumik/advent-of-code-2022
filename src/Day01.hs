module Day01 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.List (sort, reverse)

solution = Solution "day01" "" run

run input = (part1 input, part2 input)

part1 :: String -> Int
part1 = maximum . map sum . group

group :: String -> [[Int]]
group = map (map readNum . lines) .  splitOn "\n\n"

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . map sum . group
