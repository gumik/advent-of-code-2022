module Day21 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Debug.Trace (traceShow, trace)
import Control.Monad.State (State, gets, modify, evalState)
import Data.Map.Strict (Map, member, (!), insert, empty, fromList, lookup, delete)
import Prelude hiding (lookup)

solution = Solution "day21" "" run

run input = let
    monkeys = parse input
    in (part1 monkeys, part2 monkeys)

type Monkeys = Map String Monkey
data Monkey = Number Int | Operation String Operator String  deriving (Show, Eq)
data Operator = Add | Subtract | Multiply | Divide  deriving (Show, Eq)


parse :: String -> Monkeys
parse = fromList . map parseLine . lines

parseLine :: String -> (String, Monkey)
parseLine str = let
    parts = words str
    name = init $ head parts
    operator = parseOperator $ parts !! 2
    in if length parts == 2
        then (name, Number (readNum $ parts !! 1))
        else (name, Operation (parts !! 1) operator (parts !! 3))

parseOperator :: String -> Operator
parseOperator x = case x of
    "+" -> Add
    "-" -> Subtract
    "*" -> Multiply
    "/" -> Divide
    _   -> error $ "Invalid operator: " ++ x



type ResultCache = Map String Int
type CalcState = State ResultCache

part1 :: Monkeys -> Int
part1 monkeys = evalState (calc monkeys "root") empty

calc :: Monkeys -> String -> CalcState Int
calc monkeys name = do
    isAlreadyCalculated <- gets (name `member`)
    if isAlreadyCalculated
        then gets (! name)
        else case monkeys ! name of
            Number number -> do
                modify (insert name number)
                return number
            Operation m1 op m2 -> do
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


part2 :: Monkeys -> Int
part2 monkeys = let 
    Operation left _ right = monkeys ! "root" 
    monkeys' = delete "humn" monkeys
    expr1 = calc' $ monkeyToExpression monkeys' left
    expr2 = calc' $ monkeyToExpression monkeys' right
    Value val = expr2
    simplifications = iterate simplify (expr1, val)
    simplified = fst $ head $ dropWhile (uncurry (/=)) $ zip simplifications (tail simplifications)
    in case simplified of
        (Unknown, value) -> value
        _                -> error $ "Simplification failed. Last expression: " ++ show simplified

data Expression = Value Int | Expression Expression Operator Expression | Unknown  deriving (Show, Eq)

monkeyToExpression :: Monkeys -> String -> Expression
monkeyToExpression monkeys name = case lookup name monkeys of
    Nothing                 -> Unknown
    Just (Number n)         -> Value n
    Just (Operation a op b) -> Expression a' op b' where
        a' = monkeyToExpression monkeys a
        b' = monkeyToExpression monkeys b

calc' :: Expression -> Expression
calc' expr = case expr of
    Value n -> Value n
    Expression e1 op e2 -> let
        e1' = calc' e1
        e2' = calc' e2
        in case (e1', e2') of
            (Value v1, Value v2) -> Value $ apply v1 op v2
            _                    -> expr
    Unknown -> Unknown

simplify :: (Expression, Int) -> (Expression, Int)
simplify (expr, val) = case expr of
    Expression (Value x) Add      expr'      -> (expr', val - x)
    Expression expr'     Add      (Value x)  -> (expr', val - x)
    Expression (Value x) Multiply expr'      -> (expr', val `div` x)
    Expression expr'     Multiply (Value x)  -> (expr', val `div` x)
    Expression (Value x) Subtract expr'      -> (expr', x - val)     -- x - expr' = val   <=>   -expr' = val - x   <=>   expr' = x - val
    Expression expr'     Subtract (Value x)  -> (expr', val + x)
    Expression (Value x) Divide   expr'      -> (expr', x `div` val) -- x / expr' = val   <=>   x = val * expr'   <=>   expr' = x / val
    Expression expr'     Divide   (Value x)  -> (expr', val * x)
    Expression expr1     op       expr2      -> (Expression (calc' expr1) op (calc' expr2), val)
    _                                        -> (expr, val)
