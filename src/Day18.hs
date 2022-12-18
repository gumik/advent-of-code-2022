module Day18 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, toTriple)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import qualified Data.Set as Set

solution = Solution "day18" "" run

run input = let
    cubes = parse input
    in traceShow cubes (NoSolution, NoSolution)

parse = map parseLine . lines

parseLine = toTriple . map readNum . splitOn ","

type Cube = (Int, Int, Int)

part1 :: [Cube] -> Int
part1 cubes = let
    cubesSet = Set.fromList cubes
    in 0