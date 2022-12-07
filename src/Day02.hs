module Day02 where

import Common (Solution(Solution), NoSolution(..), readNum)

solution :: Solution Int Int
solution = Solution "day02" "" run

run :: String -> (Int, Int)
run input = (part1 input, part2 input)

part1 :: String -> Int
part1 = sum . map roundScore . parse makeStrategy1

part2 :: String -> Int
part2 = sum . map (roundScore . toStrategy1) . parse makeStrategy2

data Strategy = Strategy HandShape HandShape
data Strategy2 = Strategy2 HandShape ExpectedResult
data HandShape = Rock | Paper | Scissors deriving (Show, Enum)
data ExpectedResult = Draw | Lose | Win deriving (Show, Enum)

parse :: (Char -> Char -> a) -> String -> [a]
parse makeStrategy = map (parseLine makeStrategy) . lines

parseLine :: (Char -> Char -> a) -> String -> a
parseLine f l = let [p1, _, p2] = l in f p1 p2

makeStrategy1 :: Char -> Char -> Strategy
makeStrategy1 p1 p2 = Strategy (decodeHand p1) (decodeHand p2)

makeStrategy2 :: Char -> Char -> Strategy2
makeStrategy2 p1 p2 = Strategy2 (decodeHand p1) (decodeResult p2)


decodeHand :: Char -> HandShape
decodeHand c = case c of
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissors
    'X' -> Rock
    'Y' -> Paper
    'Z' -> Scissors
    _   -> error $ "Invalid input: " ++ [c]

decodeResult :: Char -> ExpectedResult
decodeResult c = case c of
    'X' -> Lose
    'Y' -> Draw
    'Z' -> Win
    _   -> error $ "Invalid input: " ++ [c]

roundScore :: Strategy -> Int
roundScore (Strategy opponent your) = shapeScore your + outcome opponent your

shapeScore :: HandShape -> Int
shapeScore = (+1) . fromEnum

outcome :: HandShape -> HandShape -> Int
outcome opponent your = let
    value = toEnum $ (fromEnum opponent - fromEnum your) `mod` 3 in
    case value of
        Draw -> 3
        Lose -> 0
        Win -> 6

toStrategy1 :: Strategy2 -> Strategy
toStrategy1 (Strategy2 opponent expectedResult) = Strategy opponent (whatShapeToChoose opponent expectedResult)

whatShapeToChoose :: HandShape -> ExpectedResult -> HandShape
whatShapeToChoose opponent expectedResult = toEnum $ (fromEnum opponent - fromEnum expectedResult) `mod` 3
