module Day03 where

import Common (Solution(Solution), NoSolution(..), readNum)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List.Split (chunksOf)

solution :: Solution Int Int
solution = Solution "day03" "" run

run :: String -> (Int, Int)
run input = let rackpacks = parse input
    in (part1 rackpacks, part2 rackpacks)

newtype Rackpack = Rackpack String deriving (Show)

rackpackCompartments :: Rackpack -> (String, String)
rackpackCompartments (Rackpack l)= let s = length l `div` 2 in splitAt s l

rackpackContent :: Rackpack -> String
rackpackContent (Rackpack s) = s

parse :: String -> [Rackpack]
parse = map Rackpack . lines

part1 :: [Rackpack] -> Int
part1 = sum . map duplicatePriority

duplicatePriority :: Rackpack -> Int
duplicatePriority rackpack = let
    (a, b) = rackpackCompartments rackpack
    setA = Set.fromList a
    setB = Set.fromList b
    commonElem = head $ Set.toList $ Set.intersection setA setB
    in priority commonElem

priorities :: Map.Map Char Int
priorities = Map.fromList ((['a'..'z'] ++ ['A'..'Z']) `zip` [1..])

priority :: Char -> Int
priority c = Map.findWithDefault 0 c priorities

part2 :: [Rackpack] -> Int
part2 = sum . map (priority . badge) . chunksOf 3

badge :: [Rackpack] -> Char
badge = head . Set.toList . foldl1 Set.intersection . map (Set.fromList . rackpackContent)
    
    -- let
    -- counts = Map.fromListWith (+) $ (`zip` repeat 1) $ concatMap rackpackContent rackpacks
    -- in fst $ head $ filter ((== 3) . snd) (Map.toList counts)
