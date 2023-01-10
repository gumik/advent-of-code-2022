module Common (
    Solution(..),
    NoSolution(..),
    listOfNumbers,
    (!?),
    readNum,
    readNum',
    parseComaSeparatedNums,
    toDecimal,
    parseArray,
    showArray,
    showCharArray,
    inArrayBounds,
    ShowString(..),
    toTuple,
    toTriple,
    setAt,
    range,
    arrayGetOr,
    subArray) where
import Numeric (readInt)
import Data.List.Split (splitOn, chunksOf)
import Data.Array hiding (range)

data Solution a b = Solution {
    solutionName :: String,
    solutionDescription :: String,
    solutionRun :: String -> (a, b)
}

data NoSolution = NoSolution deriving (Eq)
instance Show NoSolution where
    show NoSolution = "(no solution)"

newtype ShowString = ShowString String deriving(Eq)
instance Show ShowString where
    show (ShowString s) = s

listOfNumbers :: String -> [Int]
listOfNumbers content = map read (lines content) :: [Int]

(!?) :: [a] -> Int -> Maybe a
xs !? n = if n >= 0 && n < length xs
    then Just $ xs !! n
    else Nothing

readNum :: String -> Int
readNum = read

readNum' :: Char -> Int
readNum' c = readNum [c]

parseComaSeparatedNums :: String -> [Int]
parseComaSeparatedNums = map readNum . splitOn ","

toDecimal :: Int -> [Int] -> Int
toDecimal nary digits = sum $ zipWith (\d c -> d * nary^c) (reverse digits) [0..]

parseArray :: (Char -> a) -> String -> Array (Int, Int) a
parseArray readChar input = let
    parsedLines = map (map readChar) $ lines input
    width = maximum $ map length parsedLines
    height = length parsedLines
    -- In case some lines are shorter, extend them with space.
    extendedLines = map (\l -> l ++ replicate (width - length l) (readChar ' ')) parsedLines
    in listArray ((0, 0), (height-1, width-1)) $ concat extendedLines

showCharArray :: Array (Int, Int) Char -> String
showCharArray arr = let
    ((_, w1), (_, w2)) = bounds arr
    width = w2 - w1 + 1
    in unlines $ chunksOf width $ elems arr

showArray :: (a -> String) -> Array (Int, Int) a -> String
showArray showFunc arr = let
    ((_, w1), (_, w2)) = bounds arr
    width = w2 - w1 + 1
    in unlines $ map concat $ chunksOf width $ map showFunc $ elems arr

arrayGetOr :: Array (Int, Int) a -> (Int, Int) -> a -> a
arrayGetOr arr idx def
    | inArrayBounds arr idx  = arr ! idx
    | otherwise              = def

subArray :: Array (Int, Int) a -> ((Int, Int), (Int, Int)) -> Array (Int, Int) a
subArray arr ((minY, minX), (maxY, maxX)) = listArray ((0, 0), (maxY-minY, maxX-minX)) [arr ! (y, x) | y <- [minY..maxY], x <- [minX..maxX]]

inArrayBounds arr (y, x) = let
    ((h0, w0), (hm, wm)) = bounds arr
    in x >= w0 && y >= h0 && x <= wm && y <= hm

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)
toTuple _ = error "toTuple got list with length /= than 2"

toTriple :: [a] -> (a, a, a)
toTriple [x, y, z] = (x, y, z)
toTriple _ = error "toTuple got list with length /= than 3"

-- Returns new list with element set at specified position.
setAt :: Int -> a -> [a] -> [a]
setAt pos x l = let
    before = take pos l
    after = drop (pos + 1) l
    in before ++ [x] ++ after

range :: Int -> Int -> [Int]
range begin end
    | begin == end  = [begin]
    | otherwise     = [begin, begin + signum (end - begin) .. end]
