module Day21 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Debug.Trace (traceShow)
import Control.Monad.State (State, gets, modify, evalState)
import Data.Map.Strict (Map, member, (!), insert, empty)

solution = Solution "day21" "" run

run input = let
    monkeys = parse input
    in (part1 monkeys, NoSolution)

data Monkey = Number String Int | Operation String String Operator String  deriving (Show)
data Operator = Add | Subtract | Multiply | Divide  deriving (Show)

monkeyName :: Monkey -> String
monkeyName (Number name _) = name
monkeyName (Operation name _ _ _) = name

parse :: String -> [Monkey]
parse = map parseLine . lines

parseLine :: String -> Monkey
parseLine str = let
    parts = words str
    name = init $ head parts
    operator = parseOperator $ parts !! 2
    in if length parts == 2
        then Number name (readNum $ parts !! 1)
        else Operation name (parts !! 1) operator (parts !! 3)

parseOperator :: String -> Operator
parseOperator x = case x of
    "+" -> Add
    "-" -> Subtract
    "*" -> Multiply
    "/" -> Divide
    _   -> error $ "Invalid operator: " ++ x



type ResultCache = Map String Int
type CalcState = State ResultCache

part1 :: [Monkey] -> Int
part1 monkeys = evalState (calc monkeys "root") empty

calc :: [Monkey] -> String -> CalcState Int
calc monkeys name = do
    isAlreadyCalculated <- gets (name `member`)
    if isAlreadyCalculated
        then gets (! name)
        else case head $ filter ((==name) . monkeyName) monkeys of
            Number _ number -> do
                modify (insert name number)
                return number
            Operation _ m1 op m2 -> do
                v1 <- calc monkeys m1
                v2 <- calc monkeys m2
                let val = apply v1 op v2
                modify (insert name val)
                return val

apply :: Int -> Operator -> Int -> Int
apply a op b = case op of
    Add -> a + b
    Subtract -> a - b
    Multiply -> a * b
    Divide -> a `div` b


-- part2 :: [Monkey] -> Int
-- part2 = undefined

-- data 

-- calc' :: [Monkey] -> Int -> Bool
-- calc' monkeys humanVal = evalState (calc (Value "humn" humanVal : monkeys) "root") empty

