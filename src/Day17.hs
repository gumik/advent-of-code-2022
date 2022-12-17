module Day17 where

import Common (Solution(Solution), NoSolution(..), readNum)
import Prelude hiding (Left, Right)
import Debug.Trace (traceShow, trace)
import qualified Data.Set as Set
import Data.List (unfoldr)
import Data.Maybe (fromJust, isNothing)

solution = Solution "day17" "" run

run input = let
    moves = parse input
    in traceShow moves (0, NoSolution)

data Move = Left | Right deriving (Show)

parse :: String -> [Move]
parse = map toMove . filter (`elem`"><") where
    toMove x = case x of
        '>' -> Right
        '<' -> Left
        _   -> error $ "Invalid input: " ++ [x]

type Point = (Int, Int)
type Rock = [Point]

rocks :: [Rock]  -- FIXME: positive y is up, not down
rocks = [ [(0, 0), (1, 0), (2, 0), (3, 0)]
        , [(1, 0), (0, 1), (2, 1), (1, 2)]
        , [(2, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
        , [(0, 0), (0, 1), (0, 2), (0, 3)]
        , [(0, 0), (1, 0), (0, 1), (1, 1)] ]

type Board = Set.Set Point

simulateRock :: Board -> [Move] -> Rock -> (Board, [Move])
simulateRock board moves rock = let
    (rock', moves') = simulateRockStep board rock moves
    board' = foldr Set.insert board rock'
    in (board', moves')

-- data Iteration = Iteration Rock [Move] | StopIteration

simulateRockStep :: Board -> Rock -> [Move] -> (Rock, [Move])
simulateRockStep board rock moves = let
    rockPushed = pushRock board (head moves) rock
    rockFallenDown = fallDownRock board rockPushed
    in if isNothing rockFallenDown
        then (rockPushed, tail moves)
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
