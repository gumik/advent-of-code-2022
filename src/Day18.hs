module Day18 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, toTriple)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import qualified Data.Set as Set

solution = Solution "day18" "" run

run input = let
    cubes = parse input
    in (part1 cubes, part2 cubes)

parse :: String -> [Cube]
parse = map parseLine . lines

parseLine :: [Char] -> Cube
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


part2 :: [Cube] -> Int
part2 cubes = let
    scan = Set.fromList cubes
    ranges@(x, _, y, _, z, _) = range cubes
    water = waterPoints scan ranges [(x, y, z)] Set.empty
    in sum $ map (visibleSides' water) cubes

range :: [Cube] -> (Int, Int, Int, Int, Int, Int)
range cubes = let
    minX = minimum (map (\(x, y, z) -> x) cubes) - 1
    maxX = maximum (map (\(x, y, z) -> x) cubes) + 1
    minY = minimum (map (\(x, y, z) -> y) cubes) - 1
    maxY = maximum (map (\(x, y, z) -> y) cubes) + 1
    minZ = minimum (map (\(x, y, z) -> z) cubes) - 1
    maxZ = maximum (map (\(x, y, z) -> z) cubes) + 1
    in (minX, maxX, minY, maxY, minZ, maxZ)

waterPoints :: Scan -> (Int, Int, Int, Int, Int, Int) -> [Cube] -> Scan -> Scan
waterPoints _ _ [] water = water
waterPoints scan ranges@(minX, maxX, minY, maxY, minZ, maxZ) (cube@(x, y, z):rest) water
    | x < minX || x > maxX || y < minY || y > maxY || z < minY || z > maxY || cube `Set.member` water || cube `Set.member` scan  = waterPoints scan ranges rest water
    | otherwise  = waterPoints scan ranges (neighbours cube ++ rest) (cube `Set.insert` water)

visibleSides' :: Scan -> Cube -> Int
visibleSides' water cube = length $ filter id $ map (`Set.member` water) (neighbours cube)
