{-# LANGUAGE LambdaCase #-}
module Day22 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, showArray, inArrayBounds, subArray)
import Data.List.Split (splitOn)
import Data.Char (isNumber)
import Debug.Trace (traceShow, trace)
import Data.Array (Array, bounds, (!), elems)
import Prelude hiding (Left, Right)
import Data.List (nub, sort)

{-
The idea is to split input map into 6 square maps. Then do mapping for each map/edge.
For example, having input map:

   ...
   ...
   ...
............
............
............
   ...
   ...
   ...

Such 6 individual maps may be created (arranged as in input):

      +---+
      | 1 |
      +---+
+---+ +---+ +---+ +---+ 
| 2 | | 3 | | 4 | | 5 |
+---+ +---+ +---+ +---+
      +---+
      | 6 |
      +---+

Then mapping is of form:

(map, side) -> (destination map, destination side, rotation)

where side is U, D, L, R for Up, Down Left, Right
and rotation is number of how much clockwise turns are needed to have correct facing in destination map.

There are obvious starting mappings for neighbouring maps:

(1, D) -> (3, U, 0)
(2, R) -> (3, L, 0)
(3, L) -> (2, R, 0)
(3, U) -> (1, D, 0)
(3, R) -> (4, L, 0)
(3, D) -> (6, U, 0)
(4, L) -> (3, R, 0)
(4, R) -> (5, L, 0)
(5, L) -> (4, R, 0)
(6, U) -> (3, D, 0)

There are always 10 such mappings, but they may differ depending on input map arrangement.
From such mappings rest 14 mappings may be found in the following way:

1. To find mapping in map X on side Y
2. Go to map from side clockwise from Y. Call it map X' and side Y'.
3. Go to map from side clockwise from Y'. Call it map X" and side Y".
4. Destination side is clockwise from Y". The rotation is (-3 - rot1 - rot2 mod 4), where rot1 and rot2 are rotations from mapping in steps 2 and 3.
5. The opposite mapping is (X, Y, -rotation mod 4)

For example, find mapping for (4, D).

    (4, D) -> (4, L) -> (3, R, 0) -> 
              (3, D) -> (6, U, 0) -> (6, R)
    
    Rotation is -3 - 0 - 0 mod 4 = 1.
    So, final mapping is:

    (4, D) -> (6, R, 1)

    Opposite mapping is:

    (6, R) -> (4, D, 3)

Uh. Is rotation in mapping really needed? If there is mapping R -> D it is known that one clockwise turn is needed.

-}

solution = Solution "day22" "" run

run input = let
    (forceField, path) = parseInput input
    in (part1 forceField path, part2 forceField path)

type ForceField = Array (Int, Int) Field
type Path = [Instruction]
data Instruction = Move Int | Turn Rotation  deriving (Show)
data Rotation = Clockwise | CounterClockwise  deriving (Show)
data Field = None | Empty | Wall  deriving (Show, Eq)

data SquareField = SquareField {
    squareFieldOffset :: Point,
    squareFieldForceField :: ForceField }

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

toSquareFields :: ForceField -> [SquareField]
toSquareFields forceField = let
    ((minY, minX), (maxY, maxX)) = bounds forceField
    edgeCoords = sort $ nub $ [x | y <- [minY..maxY-1], x <- [minX..maxX-1], isEdge forceField (y, x) (y, x+1)]
    squareSize = head $ zipWith (-) (tail edgeCoords) edgeCoords
    squaresPositions = [(y, x) | y <- [minY, minY+squareSize .. maxY], x <- [minX, minX + squareSize .. maxX]]
    squares = [SquareField (y, x) $ subArray forceField ((y, x), (y + squareSize - 1, x + squareSize - 1)) | (y, x) <- squaresPositions]
    nonEmptySquares = filter ((/=None) . head . elems . squareFieldForceField) squares
    in traceShow (map squareFieldOffset nonEmptySquares) nonEmptySquares
    -- find horizontal and vertical coordinates to split
    -- divide whole space to SquareFields
    -- filter out the ones that contains Empty only

isEdge :: ForceField -> Point -> Point -> Bool
isEdge forceField a b = case (forceField ! a, forceField ! b) of
    (None, None) -> False
    (_, None)    -> True
    (None, _)    -> True
    _            -> False

part1 :: ForceField -> Path -> Int
part1 forceField path = 1000 * (y+1) + 4 * (x+1) + fromEnum facing where
    ((y, x), facing) = last $ goPath forceField path

part2 :: ForceField -> Path -> Int
part2 forceField path = let
    squareFields = toSquareFields forceField
    in length squareFields

type Point = (Int, Int)
data Facing = Right | Down | Left | Up  deriving (Show, Enum)

goPath :: ForceField -> Path -> [(Point, Facing)]
goPath forceField = scanl (move forceField) ((0, 0), Right)

move :: ForceField -> (Point, Facing) -> Instruction -> ((Int, Int), Facing)
move _ (pos, facing) (Turn rotation) = (pos, rotate rotation facing)
move forceField (pos, facing) (Move n) = (lastPos, facing) where
    nextPositions = iterate (advance forceField facing) pos
    positionsTypes = map (forceField !) nextPositions
    lastPos = fst $ last $ takeWhile ((/= Wall) . snd) $ take (n+1) $ filter ((/= None) . snd) (nextPositions `zip` positionsTypes)

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
