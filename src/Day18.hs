module Day18 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, toTriple)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import qualified Data.Set as Set

solution = Solution "day18" "" run

run input = let
    cubes = parse input
    in (part1 cubes, NoSolution)

parse = map parseLine . lines

parseLine = toTriple . map readNum . splitOn ","

type Cube = (Int, Int, Int)
type Scan = Set.Set Cube

part1 :: [Cube] -> Int
part1 cubes = let
    cubesSet = Set.fromList cubes
    in sum $ map (visibleSides cubesSet) cubes

neighbours :: Cube -> [Cube]
neighbours (x, y, z) = [ (x+1, y, z)
                       , (x-1, y, z)
                       , (x, y+1, z)
                       , (x, y-1, z)
                       , (x, y, z+1)
                       , (x, y, z-1) ]

visibleSides :: Scan -> Cube -> Int
visibleSides scan cube = length $ filter id $ map (`Set.notMember` scan) (neighbours cube)
