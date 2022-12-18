module Day18 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, toTriple)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

solution = Solution "day18" "" run

run input = let
    cubes = parse input
    in traceShow cubes (NoSolution, NoSolution)

parse = map parseLine . lines

parseLine = toTriple . map readNum . splitOn ","
