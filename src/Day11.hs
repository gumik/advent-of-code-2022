{-# LANGUAGE BangPatterns #-}
module Day11 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, setAt)
import Data.List.Split (splitOn)
import Prelude hiding (round)
import Data.List (transpose, sort, foldl', iterate')
import Debug.Trace (trace)

solution = Solution "day11" "" run

run input = let
    monkeys = parse input
    in (part1 monkeys, 0)

data Monkey = Monkey { monkeyItems :: [Item]
                     , monkeyOperation :: Operation
                     , monkeyTest :: Test
                     , monkeyAction :: Action
                     , monkeyProcessed :: Int}

type Item =  ModInt
data Operation = Add Int | Multiply Int | Square  deriving (Show)
type Test =  Int
type Action = (Int, Int)

bases :: [Int]
bases = [2, 3, 5, 7, 11, 13, 17, 19, 23]
newtype ModInt = ModInt [Int]

modIntList :: ModInt -> [Int]
modIntList (ModInt x) = x

toModInt :: Int -> ModInt
toModInt x = ModInt $ map (x `mod`) bases

isDivisible :: ModInt -> Int -> Bool
isDivisible n x = remainder == 0 where
    remainder = modIntList n !! baseIdx x

modIntAdd :: ModInt -> Int -> ModInt
modIntAdd n x = ModInt $ zipWith addRemainder bases (modIntList n) where
    addRemainder b r = (r + x) `mod` b

modIntMultiply :: ModInt -> Int -> ModInt
modIntMultiply n x = modIntMultiply' n (toModInt x)

modIntMultiply' :: ModInt -> ModInt -> ModInt
modIntMultiply' n m = ModInt $ map mult $ zip3 bases (modIntList n) (modIntList m) where
    mult (b, n', m') = (n' * m') `mod` b

modIntSquare :: ModInt -> ModInt
modIntSquare n = modIntMultiply' n n

baseIdx :: Int -> Int
baseIdx x = fst $ head $ filter ((== x) . snd) $ [0..] `zip` bases

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
parseStartingItems = map (toModInt . readNum) . splitOn "," . tail . dropWhile (/= ':')

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

-- TODO: Part 2 is brutally applied over part 1. Please fix me and make one code.
part1 monkeys = let
    r = iterate' round monkeys !! 10000
    counts = reverse $ sort $ map monkeyProcessed r
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
    !worryLevel = applyOperation operation item
    in if isDivisible worryLevel test
        then (fst action, worryLevel)
        else (snd action, worryLevel)

applyOperation :: Operation -> ModInt -> ModInt
applyOperation op x = case op of
  Add n -> modIntAdd x n
  Multiply n -> modIntMultiply x n
  Square -> modIntSquare x

addItem :: Monkey -> Item -> Monkey
addItem !monkey !item = let
    !items = monkeyItems monkey
    in monkey { monkeyItems = items ++ [item] }
