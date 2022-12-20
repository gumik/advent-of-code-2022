module Day20 where

import Common (Solution(Solution), NoSolution(..), readNum)
import Debug.Trace (traceShow)

solution :: Solution Int Int
solution = Solution "day20" "" run

run :: String -> (Int, Int)
run input = let
    numbers = parse input
    in (part1 numbers, part2 numbers)

parse :: String -> [Int]
parse = map readNum . lines

atPos :: [Int] -> Int -> Int
atPos l n = l !! (n `mod` length l)


part1 :: [Int] -> Int
part1 numbers = let
    mixed = map snd $ mix 0 ([0..] `zip` numbers)
    zeroPos = length $ takeWhile (/=0) mixed
    len = length mixed
    in sum [atPos mixed (zeroPos + 1000), atPos mixed (zeroPos + 2000), atPos mixed (zeroPos + 3000)]

part2 :: [Int] -> Int
part2 numbers = let
    mixed1 = mix 0 ([0..] `zip` map (*811589153) numbers)
    mixed2 = mix 0 mixed1
    mixed3 = mix 0 mixed2
    mixed4 = mix 0 mixed3
    mixed5 = mix 0 mixed4
    mixed6 = mix 0 mixed5
    mixed7 = mix 0 mixed6
    mixed8 = mix 0 mixed7
    mixed9 = mix 0 mixed8
    mixed10 = map snd $ mix 0 mixed9
    zeroPos = length $ takeWhile (/=0) mixed10
    len = length mixed10
    in sum [atPos mixed10 (zeroPos + 1000), atPos mixed10 (zeroPos + 2000), atPos mixed10 (zeroPos + 3000)]


-- Very inefficent, but gives result in 10 seconds.

mix :: Int -> [(Int, Int)] -> [(Int, Int)]
mix n nums = if n >= length nums then nums else let
    (prev, (_, x):next) = break ((==n) . fst) nums
    actualPos = length prev
    nums' = prev ++ next
    newPos = (actualPos + x) `mod` length nums'
    in mix (n+1) $ take newPos nums' ++ [(n, x)] ++ drop newPos nums'
