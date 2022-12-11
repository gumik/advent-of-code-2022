{-# LANGUAGE BangPatterns #-}
module Day11 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, setAt)
import Data.List.Split (splitOn)
import Prelude hiding (round)
import Data.List (transpose, sort, foldl', iterate')

solution = Solution "day11" "" run

run input = let
    monkeys = parse input
    in (part1 monkeys, 0)

data Monkey = Monkey { monkeyItems :: [Item]
                     , monkeyOperation :: Operation
                     , monkeyTest :: Test
                     , monkeyAction :: Action 
                     , monkeyProcessed :: Int}

type Item =  Int
data Operation = Add Int | Multiply Int | Square  deriving (Show)
type Test =  Int
type Action = (Int, Int)


parse :: String -> [Monkey]
parse = map parseMonkey . splitOn "\n\n"

parseMonkey :: String -> Monkey
parseMonkey input = let
    [_, startingItemsStr, operationStr, testStr, actionTrueStr, actionFalseStr] = lines input
    in Monkey (parseStartingItems startingItemsStr)
              (parseOperation operationStr)
              (parseTest testStr)
              (parseAction actionTrueStr, parseAction actionFalseStr)
              0

parseStartingItems :: String -> [Item]
parseStartingItems = map readNum . splitOn "," . tail . dropWhile (/= ':')

parseOperation :: String -> Operation
parseOperation str = let
    w = words str
    opStr = last $ init w
    x = readNum $ last w
    in case opStr of
        "+" -> Add x
        "*" -> case last w of
            "old" -> Square
            _     -> Multiply x
        _         -> error $ "Invalid operation: " ++ str

parseTest :: String -> Test
parseTest = readLastNum

parseAction :: String -> Int
parseAction = readLastNum

readLastNum :: String -> Int
readLastNum = readNum . last . words


part1 monkeys = let
    rounds = take 21 $ iterate' round monkeys
    counts = reverse $ sort $ last $ map (map monkeyProcessed) rounds
    in product $ take 2 counts

round :: [Monkey] -> [Monkey]
round !monkeys = foldl' roundForMonkey monkeys [0..length monkeys-1]

roundForMonkey :: [Monkey] -> Int -> [Monkey]
roundForMonkey !monkeys !n = let
    !monkey = monkeys !! n
    !itemsToTransfer = turn monkey
    !processed = monkeyProcessed monkey
    !monkey' = monkey { monkeyItems = [], monkeyProcessed = processed + length (monkeyItems monkey) }
    !monkeys' = setAt n monkey' monkeys
    in foldl' addItemsToMonkeyAtIdx monkeys' itemsToTransfer

addItemsToMonkeyAtIdx :: [Monkey] -> (Int, Item) -> [Monkey]
addItemsToMonkeyAtIdx !monkeys (!i, !item) = setAt i (addItem (monkeys !! i) item) monkeys

turn :: Monkey -> [(Int, Item)]
turn (Monkey !items !operation !test !action !_) = map (monkeyTurn operation test action) items

monkeyTurn :: Operation -> Test -> Action -> Item -> (Int, Item)
monkeyTurn !operation !test !action !item = let
    !worryLevel = applyOperation operation item `div` 3
    in if worryLevel `mod` test == 0
        then (fst action, worryLevel)
        else (snd action, worryLevel)

applyOperation :: Operation -> Int -> Int
applyOperation op x = case op of
  Add n -> x + n
  Multiply n -> x * n
  Square -> x * x

addItem :: Monkey -> Item -> Monkey
addItem !monkey !item = let
    !items = monkeyItems monkey
    in monkey { monkeyItems = items ++ [item] }
