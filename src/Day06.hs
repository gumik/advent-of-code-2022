module Day06 where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List (tails, nub)

solution :: Solution Int Int
solution = Solution "day06" "" run

run :: String -> (Int, Int)
run input = (solve 4 input, solve 14 input)

solve :: Int -> String -> Int
solve n = fst . head . filter ((== n) . length . snd) . zip [n..] . map (nub . take n) . tails
