{-# LANGUAGE LambdaCase #-}
module Day22 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, showArray, inArrayBounds)
import Data.List.Split (splitOn)
import Data.Char (isNumber)
import Debug.Trace (traceShow, trace)
import Data.Array (Array, bounds, (!))
import Prelude hiding (Left, Right)

solution = Solution "day22" "" run

run input = let
    (forceField, path) = parseInput input
    in (part1 forceField path, NoSolution)

type ForceField = Array (Int, Int) Field
type Path = [Instruction]
data Instruction = Move Int | Turn Rotation  deriving (Show)
data Rotation = Clockwise | CounterClockwise  deriving (Show)
data Field = None | Empty | Wall  deriving (Show, Eq)

parseInput :: String -> (ForceField, Path)
parseInput input = let
    [forceFieldStr, pathStr] = splitOn "\n\n" input
    in (parseForceField forceFieldStr, parsePath $ filter (/= '\n') pathStr)

parseForceField :: String -> ForceField
parseForceField = parseArray (\case
    '#' -> Wall
    '.' -> Empty
    _   -> None)

parsePath :: String -> Path
parsePath [] = []
parsePath str@(x:xs) = case x of
    'R' -> Turn Clockwise : parsePath xs
    'L' -> Turn CounterClockwise : parsePath xs
    _   -> Move (read numStr) : parsePath xs where
        (numStr, xs) = span isNumber str


part1 :: ForceField -> Path -> Int
part1 forceField path = let
    moves = goPath forceField path
    ((y, x), facing) = last moves
    in traceShow moves $ 1000 * (y+1) + 4 * (x+1) + fromEnum facing

type Point = (Int, Int)
data Facing = Right | Down | Left | Up  deriving (Show, Enum)

goPath :: ForceField -> Path -> [(Point, Facing)]
goPath forceField path = let
    ((minX, minY), (maxX, maxY)) = bounds forceField
    --initialPosX = minimum [x | x <- [minX..maxX], forceField ! (0, x) /= None]
    in scanl (move forceField) ((0, 0), Right) path

move :: ForceField -> (Point, Facing) -> Instruction -> ((Int, Int), Facing)
move _ (pos, facing) (Turn rotation) = (pos, rotate rotation facing)
move forceField (pos, facing) (Move n) = (lastPos, facing) where
    nextPositions = iterate (advance forceField facing) pos
    counter = scanl count n nextPositions
    count n pos = trace ("n == " ++ show n ++ ", forceField ! " ++ show pos ++ " == " ++ show (forceField ! pos)) $ case forceField ! pos of
        None  -> n
        Empty -> n-1
        Wall  -> -1
    lastPos = fst $ last $ takeWhile (\(pos, n) -> n >= 0 && forceField ! pos /= Wall) $ filter ((/= None) . (forceField !) . fst) $ nextPositions `zip` counter
    
    -- case n of
    -- 0 -> (pos, facing)
    -- _ -> let nextPos = advance forceField pos facing
    --     in case forceField ! nextPos of
    --         None  -> move forceField (nextPos, facing) (Move n)
    --         Empty -> move forceField (nextPos, facing) (Move (n-1))
    --         Wall  -> (pos, facing)

advance :: ForceField -> Facing -> Point -> Point
advance forceField facing (y, x) = (y' `mod` (maxY+1), x' `mod` (maxX+1)) where
    (_, (maxY, maxX)) = bounds forceField
    (y', x') = case facing of
        Left  -> (y, x-1)
        Up    -> (y-1, x)
        Right -> (y, x+1)
        Down  -> (y+1, x)

rotate :: Rotation -> Facing -> Facing
rotate rotation facing = toEnum $ (fromEnum facing + dir) `mod` 4 where
    dir = case rotation of
        Clockwise        ->  1
        CounterClockwise -> -1

