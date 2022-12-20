module Day17 where

import Common (Solution(Solution), NoSolution(..), readNum, showArray)
import Prelude hiding (Left, Right)
import Debug.Trace (traceShow, trace)
import qualified Data.Set as Set
import Data.List (unfoldr)
import Data.Maybe (fromJust, isNothing)
import Data.Array (array)

solution = Solution "day17" "" run

run input = let
    moves = parse input
    in (part1 moves, NoSolution)

data Move = Left | Right deriving (Show)

parse :: String -> [Move]
parse = map toMove . filter (`elem`"><") where
    toMove x = case x of
        '>' -> Right
        '<' -> Left
        _   -> error $ "Invalid input: " ++ [x]

type Point = (Int, Int)
type Rock = [Point]

part1 :: [Move] -> Int
part1 moves = let
    (finalBoard, _, _) = iterate simulateRocks (Set.empty, cycle moves, cycle rocks) !! 2022
    in 1 + maximum (map snd $ Set.toList finalBoard)

showBoard :: Board -> String
showBoard board = let
    points = Set.toList board
    height = maximum $ map snd points
    in (showArray id $ array ((0, 0), (height, 6)) $ [((y, x), " ") | x <- [0..6], y <- [0..height]] ++ [((height-y, x), "#") | (x, y) <- points])
        ++ "-------\n"

rocks :: [Rock]  -- Left side two points away from left edge, bottom is at level 0.
rocks = [ [(2, 0), (3, 0), (4, 0), (5, 0)]
        , [(3, 0), (2, 1), (4, 1), (3, 2)]
        , [(2, 0), (3, 0), (4, 0), (4, 1), (4, 2)]
        , [(2, 0), (2, 1), (2, 2), (2, 3)]
        , [(2, 0), (3, 0), (2, 1), (3, 1)] ]

type Board = Set.Set Point

simulateRocks :: (Board, [Move], [Rock]) -> (Board, [Move], [Rock])
simulateRocks (board, moves, rocks) = let
    top = maximum $ ((-1):) $ map snd $ Set.toList board
    rock = rockToTop (head rocks) top
    (board', moves') = simulateRock board moves rock
    in  (board', moves', tail rocks)

rockToTop :: Rock -> Int -> Rock
rockToTop rock n = map (\(x, y) -> (x, y + n + 4)) rock

simulateRock :: Board -> [Move] -> Rock -> (Board, [Move])
simulateRock board moves rock = (board', moves') where
    (board', rock', moves') = simulateRockStep board rock moves

simulateRockStep :: Board -> Rock -> [Move] -> (Board, Rock, [Move])
simulateRockStep board rock moves = let
    rockPushed = pushRock board (head moves) rock
    rockFallenDown = fallDownRock board rockPushed
    in if isNothing rockFallenDown
        then (foldr Set.insert board rockPushed, rockPushed, tail moves)
        else simulateRockStep board (fromJust rockFallenDown) (tail moves)

pushRock :: Board -> Move -> Rock -> Rock
pushRock board move rock = let
    rock' = case move of
        Left -> map (\(x, y) -> (x - 1, y)) rock
        Right -> map (\(x, y) -> (x + 1, y)) rock
    in if any (\(x, y) -> x < 0 || x > 6 || (x, y) `Set.member` board) rock' then rock else rock'

fallDownRock :: Board -> Rock -> Maybe Rock
fallDownRock board rock = let
    rock' = map (\(x, y) -> (x, y - 1)) rock
    in if any (\(x, y) -> y < 0 || (x, y) `Set.member` board) rock' then Nothing else Just rock'
