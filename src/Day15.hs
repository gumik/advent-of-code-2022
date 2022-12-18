module Day15 where

import Common (Solution(Solution), NoSolution(..), readNum)
import Debug.Trace (traceShow)
import Control.Arrow (ArrowChoice(left))

solution = Solution "day15" "" run

run input = let
    pairs = parse input
    in {- traceShow pairs -} (part1 pairs, part2 pairs)

type Point = (Int, Int)
type SensorBeacon = (Point, Point)

parse :: String -> [SensorBeacon]
parse = map parseLine . lines

parseLine :: String -> SensorBeacon
parseLine str = let
    parts = words str
    in ((parseCoord $ parts !! 2, parseCoord $ parts !! 3), (parseCoord $ parts !! 8, parseCoord $ parts !! 9))

parseCoord :: String -> Int
parseCoord = readNum . filter (`elem` "0123456789-")


part1 :: [SensorBeacon] -> Int
part1 l = let
    sensors = map toSensor l
    beacons = map snd l
    leftBound = minimum $ map (\(Sensor (x, _) coverage) -> x - coverage) sensors
    rightBound = maximum $ map (\(Sensor (x, _) coverage) -> x + coverage) sensors
    lineCoord = 2000000
    coverageOnLine = length $ filter id [coveredByAnySensor sensors (x, lineCoord) | x <- [leftBound..rightBound], (x, lineCoord) `notElem` beacons]
    in {- traceShow (leftBound, rightBound) -} coverageOnLine

data Sensor = Sensor { sensorPos :: Point, sensorCoverage :: Int }  deriving (Show)

toSensor :: SensorBeacon -> Sensor
toSensor (s, b) = Sensor s (distance s b)

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

coveredByAnySensor :: [Sensor] -> Point -> Bool
coveredByAnySensor sensors point = any (coveredBySensor point) sensors

coveredBySensor :: Point -> Sensor -> Bool
coveredBySensor point (Sensor sensorPoint coverage) = distance point sensorPoint <= coverage

{-
 The idea is to rotate things by 45 degree. This way it should be convienient to use similar solution
 as in AoC 2021 day 22, but in 2 dimensions.
 
 (x, y) -> (x + y, y - x)   (and back (x, y) -> ((x - y) / 2, (x + y) / 2), ignore non-integral)
 Note that after rotation there is a lot more points, eg.

   0 1 2 3 4 5         1 2 3 4 5 6 7
 0     #            -3 
 1   # # #      =>  -2   # . # . #
 2 # # # # #        -1   . # . # .
 3   # # #           0   # . # . #
 4     #             1   . # . # .
                     2   # . # . #

 By dot marked points that are part of rectangle in new space, but are not existing in old space.
 That is no problem. After mapping back all this points are forgotten.
-}

part2 :: [SensorBeacon] -> Int
part2 l = let
    sensors = map toSensor l
    sensorsCorners = map corners sensors
    rotated = map (\(p1, p2) -> (rotate p1, rotate p2)) sensorsCorners
    leftBound = minimum $ map fst $ concatMap (\(p1, p2) -> [p1, p2]) rotated
    rightBound = maximum $ map fst $ concatMap (\(p1, p2) -> [p1, p2]) rotated
    upBound = minimum $ map snd $ concatMap (\(p1, p2) -> [p1, p2]) rotated
    downBound = maximum $ map snd $ concatMap (\(p1, p2) -> [p1, p2]) rotated
    segments = map (\((x1, y1), (x2, y2)) -> Segment x1 x2 [Segment y1 y2 [Unit]]) rotated
    xxx = foldl remove [Segment leftBound rightBound [Segment upBound downBound [Unit]]] segments
    in {- traceShow xxx $ -} tuningFrequency (3135800,2766584)

{- Based on this debug easily found manually:
   Segment 5902383 5902385 [Segment (-4891148) (-3061323) [Unit],Segment (-369217) (-369215) [Unit],Segment 3293187 4393282 [Unit]],
   So the point is (5902384, -369216)
   Rotated back: (3135800,2766584) -}

{- TODO: clean me
   Maybe easiest approach is to find lines that close a point in between from all directions?
   To do that also rotation would be used. Then find such lines in x direction, then y direction.
   May not work if the point is surrounded by corners.
  
   # # | # | # #
   - - + - + - -
   # # |   | # #
   - - + - + - - 
   # # | # | # #
-}
 

rotate :: Point -> Point
rotate (x, y) = (x + y, y - x)

rotate' :: Point -> Point
rotate' (x, y) = ((x - y) `div` 2, (x + y) `div` 2)

corners :: Sensor -> (Point, Point)
corners (Sensor (x, y) coverage) = ((x, y-coverage), (x, y+coverage))

tuningFrequency (x, y) = x * 4000000 + y

data Segment = Segment Int Int [Segment] | Unit deriving (Show, Eq)

add :: [Segment] -> Segment -> [Segment]
add segments Unit = segments
add [] segment = [segment]
add segments a@(Segment a1 a2 [subA]) = merge partA partB where
    partB = splitSegments segments (indices [a])
    partA = splitSegments [a] (indices segments)
add segments segment = error $ "unhandled case in add: " ++ show segments ++ ", " ++ show segment

remove :: [Segment] -> Segment -> [Segment]
remove [Unit] Unit = []
remove segments Unit = segments
remove [] segment = []
remove segments a@(Segment a1 a2 [subA]) = {- trace ("merge' " ++ show partA ++ ", " ++ show partB) $ -} merge' partA partB where
    partB = splitSegments segments (indices [a])
    partA = splitSegments [a] (indices segments)
remove segments segment = error $ "unhandled case in remove: " ++ show segments ++ ", " ++ show segment

splitSegments :: [Segment] -> [Int] -> [Segment]
splitSegments [] _ = []
splitSegments segs [] = segs
splitSegments segs@(seg@(Segment a1 a2 sub) : rest) idxs@(x:xs)
    | x <= a1    = splitSegments segs xs
    | x < a2     = Segment a1 x sub : splitSegments (Segment x a2 sub : rest) xs
    | otherwise  = seg : splitSegments rest idxs

merge :: [Segment] -> [Segment] -> [Segment]
merge [] segments = segments
merge segments [] = segments
merge segsA@(a@(Segment a1 a2 [subA]):restA) segsB@(b@(Segment b1 b2 subB):restB)
    | a1 < b1    = a : merge restA segsB
    | b1 < a1    = b : merge segsA restB
    | otherwise  = (Segment a1 b2 (add subB subA)) : merge restA restB

merge' :: [Segment] -> [Segment] -> [Segment]
merge' [] segments = segments
merge' segments [] = []
merge' segsA@(a@(Segment a1 a2 [subA]):restA) segsB@(b@(Segment b1 b2 subB):restB)
    | a1 < b1    = merge' restA segsB
    | b1 < a1    = b : merge' segsA restB
    | otherwise  = case remove subB subA of
        [] -> merge' restA restB
        newSub -> (Segment a1 b2 newSub) : merge' restA restB


indices :: [Segment] -> [Int]
indices = uniq . concat . map segIndices

uniq :: Eq a => [a] -> [a]
uniq (a:b:xs) = if a == b then a:uniq xs else a:b:uniq xs
uniq xs = xs

segIndices :: Segment -> [Int]
segIndices (Segment a b _) = [a, b]
