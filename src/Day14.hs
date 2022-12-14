module Day14 where

import Common (Solution(Solution), NoSolution(..), readNum, toTuple, range)
import Data.List.Split (splitOn)
import Data.List (nub)
import Debug.Trace (traceShow, trace)
import qualified Data.Set as Set

solution = Solution "day14" "" run

run input = let
    parsedLines = parse input
    points = nub $ concatMap linePoints parsedLines
    pointsMap = Set.fromList points
    in (part1 pointsMap, part2 pointsMap)

type Line = [Point]
type Point = (Int, Int)

parse :: String -> [Line]
parse = map parseLine . lines

parseLine :: String -> Line
parseLine = map (toTuple . map readNum . splitOn ",") . splitOn " -> "


part1 :: PointsMap -> Int
part1 pointsMap = let
    abbysLevel = maximum $ Set.map snd pointsMap
    iterations = iterate (simulateUnitOfSand abbysLevel) pointsMap
    lastIteration = fst $ head $ dropWhile (uncurry (/=)) $ zip iterations (tail iterations)
    in Set.size lastIteration - length pointsMap

part2 :: PointsMap -> Int
part2 pointsMap = let
    lowestRockLevel = maximum $ Set.map snd pointsMap
    floorLevel = 2 + lowestRockLevel
    floorPoints = Set.fromList [(x, floorLevel) | x <- [500-floorLevel-1 .. 500+floorLevel+1]]
    pointsMap' = pointsMap `Set.union` floorPoints
    iterations = iterate (simulateUnitOfSand floorLevel) pointsMap'
    lastIteration = fst $ head $ dropWhile (uncurry (/=)) $ zip iterations (tail iterations)
    in Set.size lastIteration - length pointsMap'

linePoints :: Line -> [Point]
linePoints line = concat $ zipWith expand line (tail line) where
    expand (b1, b2) (e1, e2) = [(a, b) | a <- range b1 e1, b <- range b2 e2]

type PointsMap = Set.Set Point

simulateUnitOfSand :: Int -> PointsMap -> PointsMap
simulateUnitOfSand abbysLevel pm = let
    steps = iterate (simulateUnitOfSandStep pm) (500, 0)
    point' = fst $ head $ dropWhile (\(p1, p2) -> p1 /= p2 && not (isAbbys abbysLevel p1)) $ zip steps (tail steps)
    in if isAbbys abbysLevel point'
        then pm
        else point' `Set.insert` pm

simulateUnitOfSandStep :: PointsMap -> Point -> Point
simulateUnitOfSandStep pm point
    | pointDown point `Set.notMember` pm        = pointDown point
    | pointSlideLeft point `Set.notMember` pm   = pointSlideLeft point
    | pointSlideRight point `Set.notMember` pm  = pointSlideRight point
    | otherwise                                 = point

pointDown :: Point -> Point
pointDown (x, y) = (x, y+1)

pointSlideLeft :: Point -> Point
pointSlideLeft (x, y) = (x-1, y+1)

pointSlideRight :: Point -> Point
pointSlideRight (x, y) = (x+1, y+1)

isAbbys :: Int -> Point -> Bool
isAbbys abbysLevel (_, y) = y >= abbysLevel
