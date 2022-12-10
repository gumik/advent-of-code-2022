module Day10 where

import Common (Solution(Solution), NoSolution(..), readNum, ShowString(..))
import Data.List.Split (chunksOf)
import Data.List (intercalate)

solution = Solution "day10" "" run

run input = let
    commands = parse input
    registerStates = cycles 1 commands
    in (part1 registerStates, part2 registerStates)

parse :: String -> [Cmd]
parse = map parseLine . lines

data Cmd = Noop | Addx Int deriving (Show)

parseLine :: String -> Cmd
parseLine line = case take 4 line of
    "noop" -> Noop
    "addx" -> Addx (readNum $ drop 5 line)
    _      -> error $ "Invalid input: " ++ line


part1 :: [Int] -> Int
part1 registerStates = sum (map (\idx -> idx * (registerStates !! (idx-1))) [20, 60 .. 220])

part2 :: [Int] -> ShowString
part2 = ShowString . intercalate "\n" . chunksOf 40 . zipWith drawPixel [0..239]

drawPixel :: Int -> Int -> Char
drawPixel pos sprite
    | abs ((pos `mod` 40) - sprite) <= 1  = '#'
    | otherwise                = ' '

cycles :: Int -> [Cmd] -> [Int]
cycles reg [] = [reg]
cycles reg (cmd:rest) = case cmd of
    Noop   -> reg : cycles reg rest
    Addx x -> reg : reg : cycles (x+reg) rest
