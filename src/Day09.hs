module Day09 where

import Common (Solution(Solution), NoSolution(..), readNum, range)
import Data.List.Split (splitOn)
import Prelude hiding (Right, Left)
import qualified Data.Set as Set

solution :: Solution Int Int
solution = Solution "day09" "" run

run :: String -> (Int, Int)
run input = let
    moves = parse input
    in (part1 moves, part2 moves)

data Move = Move Direction Int deriving (Show)
data Direction = Left | Right | Up | Down deriving (Show)
type Point = (Int, Int)

parse :: String -> [Move]
parse = map parseLine . lines

parseLine :: String -> Move
parseLine line = let
    [directionStr, numStr] = splitOn " " line
    in Move (parseDirection directionStr) (readNum numStr)

parseDirection :: String -> Direction
parseDirection x = case x of
    "U" -> Up
    "D" -> Down
    "L" -> Left
    "R" -> Right
    _   -> error $ "Invalid direction: " ++ x


part1 :: [Move] -> Int
part1 moves = let
    headPoints = points (0,0) moves
    in Set.size $ Set.fromList $ scanl move (0,0) headPoints

part2 :: [Move] -> Int
part2 moves = let
    headPoints = points (0,0) moves
    in Set.size $ Set.fromList $ iterate (scanl move (0,0)) headPoints !! 9


points :: Point -> [Move] -> [Point]
points p [] = [p]
points (x, y) (Move direction n : restMoves) = let
    (h, v) = case direction of
        Up    -> ( 0, -n)
        Down  -> ( 0,  n)
        Left  -> (-n,  0)
        Right -> ( n,  0)
    path = [(x', y') | x' <- range x (x+h), y' <- range y (y+v)]
    in init path ++ points (last path) restMoves

move :: Point -> Point -> Point
move tail@(t1, t2) h@(h1, h2) = let
    verticalDiff = h1 - t1
    horizontalDiff = h2 - t2
    in (t1 + diffToMove verticalDiff horizontalDiff, t2 + diffToMove horizontalDiff verticalDiff)

diffToMove :: (Eq a1, Eq a2, Num a1, Num a2, Num p) => a1 -> a2 -> p
diffToMove a b
    | (a ==  1 && abs b == 2) || a ==  2  =  1
    | (a == -1 && abs b == 2) || a == -2  = -1
    | otherwise                           =  0


-- no move
-- x 0 1 2 3 4 5
-- 0 . . . . . .
-- 1 . H H H . .
-- 2 . H T H . .
-- 3 . H H H . .
-- 4 . . . . . .

-- move
-- x 0 1 2 3 4 5 6
-- 0 . . . . . . .
-- 1 . H H H H H .
-- 2 . H . . . H .
-- 3 . H . T . H .
-- 4 . H . . . H .
-- 5 . H H H H H .
-- 6 . . . . . . .
