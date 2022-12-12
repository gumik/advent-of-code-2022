module Day12 ( solution, parse, step, initialQueue, relax ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, readNum', inArrayBounds, showArray)
import Data.Array(Array, bounds, (!), listArray, array, indices, assocs)
import qualified Data.PSQueue as Q
import Data.PSQueue (Binding ((:->)))
import GHC.Base (maxInt)
import qualified Data.Map.Strict as M
import Data.Maybe ( fromJust )
import Data.Char (ord)


solution :: Solution Int Int
solution = Solution "day12" "" run

run :: String -> (Int, Int)
run input = let
    heightmap = parse input
    endPos = findPos heightmap 'E'
    costs = findCosts heightmap endPos
    in (part1 heightmap costs, part2 heightmap costs)

type Heightmap = Array Point Char
type Neighbours = Array Point [Point]

parse :: String -> Heightmap
parse = parseArray id


part1 :: Heightmap -> BestPathCost -> Int
part1 heightmap costs = let
    startPos = findPos heightmap 'S'
    in costs M.! startPos

part2 :: Heightmap -> BestPathCost -> Int
part2 heightmap costs = let
    positions = findPos' heightmap (`elem` "Sa")
    in minimum $ map (costs M.!) positions


findCosts :: Heightmap -> Point -> BestPathCost
findCosts heightmap point = let
    neighbours = makeNeighbours heightmap
    costArray = listArray (bounds heightmap) (repeat 1)
    in pathsCosts costArray neighbours point

findPos :: Heightmap -> Char -> Point
findPos heightmap c = head $ findPos' heightmap (== c) 

findPos' :: Heightmap -> (Char -> Bool) -> [Point]
findPos' heightmap condition = map fst $ filter (condition . snd) $ assocs heightmap

makeNeighbours :: Heightmap -> Neighbours
makeNeighbours heightmap = array (bounds heightmap) (map (pointNeighbours heightmap) $ indices heightmap)

pointNeighbours :: Heightmap -> Point -> (Point, [Point])
pointNeighbours heightmap p@(y, x) = (p, filter isValidNeighbour possibleNeighbours) where
    currentHeight = heightmap ! p
    possibleNeighbours = filter (inArrayBounds heightmap) [(y, x-1), (y-1, x), (y, x+1), (y+1, x)]
    isValidNeighbour neighbour = let
        neighbourHeight = (heightmap ! neighbour)
        in height currentHeight - height neighbourHeight <= 1

height :: Char -> Int
height c = case c of
    'S' -> ord 'a'
    'E' -> ord 'z'
    _   -> ord c

-- Djikstra algorithm. Copied from AoC 2021 day 15. A little modified.
-- Each move has the same cost of 1. This should behave like BFS then, found shortest path.
-- Searching from end, to start. This solves both parts.

type Point = (Int, Int)
type PQueue = Q.PSQ Point Int
type CostArray = Array Point Int
type BestPathCost = M.Map Point Int

data Iteration = Iteration {
    iterationCostArray :: CostArray,
    iterationNeighbours :: Neighbours,
    iterationQueue :: PQueue,
    iterationBestPathCost :: BestPathCost } deriving (Show)

pathsCosts :: CostArray -> Neighbours -> Point -> BestPathCost
pathsCosts costArray neighbors startPoint = let
    bestPathCost = initialBestPathCost costArray startPoint
    queue = initialQueue bestPathCost
    Iteration _ _ _ bestPathCost' = head $ dropWhile (not . Q.null . iterationQueue) $ iterate step (Iteration costArray neighbors queue bestPathCost)
    in bestPathCost'

initialQueue :: BestPathCost -> PQueue
initialQueue = Q.fromList . map (uncurry (:->)) . M.toList

initialBestPathCost :: CostArray -> Point -> BestPathCost
initialBestPathCost costArray startPoint = M.fromList $ (startPoint, 0)  : [((y,  x), maxInt) 
                                                                           | x <- [minX..maxX]
                                                                           , y <- [minY..maxY]
                                                                           , (y, x) /= startPoint] where
    ((minY, minX), (maxY, maxX)) = bounds costArray

step :: Iteration -> Iteration
step iteration@(Iteration costArray neighbours queue bestPathCost)
    | Q.null queue = iteration
    | otherwise = let
        (point :-> cost, queue') = fromJust $ Q.minView queue
        points = neighbours ! point
        (queue'', bestPathCost') = foldl (relax costArray cost) (queue', bestPathCost) points
        in bestPathCost `seq` Iteration costArray neighbours queue'' bestPathCost'

relax :: CostArray -> Int -> (PQueue, BestPathCost) -> Point -> (PQueue, BestPathCost)
relax costArray costFrom (queue, bestPathCost) pointTo = (queue', bestPathCost') where
    queue' = Q.alter (fmap (min newCost)) pointTo queue
    bestPathCost' = M.adjust (min newCost) pointTo bestPathCost
    newCost = costTo `safeAdd` costFrom
    costTo = costArray ! pointTo

safeAdd :: Int -> Int -> Int
safeAdd a b
    | a == maxInt || b == maxInt  = maxInt
    | otherwise                   = a + b
