module Day08 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum', parseArray, showArray)
import Data.Array ( Array, bounds, (!), indices)
import qualified Data.Set as Set

solution :: Solution Int Int
solution = Solution "day08" "" run

run :: String -> (Int, Int)
run input = let
    treesMap = parse input
    in (part1 treesMap, part2 treesMap)

type Coord = (Int, Int)
type Map = Array Coord Int

parse :: String -> Map
parse = parseArray readNum'

part1 :: Map -> Int
part1 = length . visibleTrees

visibleTrees :: Map -> [Coord]
visibleTrees treesMap = let
    ((minX, minY), (maxX, maxY)) = bounds treesMap
    left =   [[(x, y) | x <- [minX..maxX]]        | y <- [minY..maxY]]
    right =  [[(x, y) | x <- [maxX,maxX-1..minX]] | y <- [minY..maxY]]
    top =    [[(x, y) | y <- [minY..maxY]]        | x <- [minX..maxX]]
    bottom = [[(x, y) | y <- [maxY,maxY-1..minY]] | x <- [minX..maxX]]
    in Set.toList $ Set.fromList $ concatMap (visibleTreesOnCoords treesMap) (left ++ right ++ top ++ bottom)

visibleTreesOnCoords :: Map -> [Coord] -> [Coord]
visibleTreesOnCoords treesMap coords = snd $ foldl (collectVisibleTree treesMap) (-1, []) coords

collectVisibleTree :: Map -> (Int, [Coord]) -> Coord -> (Int, [Coord])
collectVisibleTree treesMap (maxHeight, collected) coord = let
    height = treesMap ! coord
    in if height > maxHeight
        then (height, coord : collected)
        else (maxHeight, collected)


part2 :: Map -> Int
part2 = bestScenicScore

bestScenicScore :: Map -> Int
bestScenicScore treesMap = maximum $ map (scenicScore treesMap) $ indices treesMap

scenicScore :: Map -> Coord -> Int
scenicScore treesMap coord = let
    ((minX, minY), (maxX, maxY)) = bounds treesMap
    height = treesMap ! coord
    (pointX, pointY) = coord
    left   = [(x, pointY) | x <- [pointX-1,pointX-2..minX]]
    right  = [(x, pointY) | x <- [pointX+1..maxX]]
    top    = [(pointX, y) | y <- [pointY-1,pointY-2..minY]]
    bottom = [(pointX, y) | y <- [pointY+1..maxY]]
    in product $ map (viewingDistance treesMap height) [left, right, top, bottom]

viewingDistance :: Map -> Int -> [Coord] -> Int
viewingDistance treesMap height coords = let
    (a, b) = span ((< height) . (treesMap !)) coords
    in length a + min (length b) 1
