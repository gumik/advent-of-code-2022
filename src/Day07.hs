module Day07 where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.List (sort)

solution :: Solution Int Int
solution = Solution "day07" "" run

run :: String -> (Int, Int)
run input = let
    fs = buildFilesystem $ parse input
    sizes = dirsSizes fs
    in (part1 sizes, part2 sizes)

type Path = [String]
data Entry = File Path Int | Dir Path  deriving (Show)
type Filesystem = [Entry]

data Cmd = ChangeDir String | ListDir [Entry]  deriving (Show)

entryPath :: Entry -> Path
entryPath (File path _) = path
entryPath (Dir path) = path

prependPath :: Path -> Entry -> Entry
prependPath parentPath entry = case entry of
    Dir path -> Dir (parentPath ++ path)
    File path size -> File (parentPath ++ path) size

isDir :: Entry -> Bool
isDir entry = case entry of
    Dir _ -> True
    _     -> False

rootDir :: Entry
rootDir = Dir []

parse :: String -> [Cmd]
parse = map parseCmd . filter (/= "") . splitOn "$ "

parseCmd :: String -> Cmd
parseCmd cmdStr = let
    parts = lines cmdStr
    part1 = head parts
    in case head $ splitOn " " part1 of
        "ls" -> parseLs $ tail parts
        "cd" -> parseCd part1
        _    -> error $ "Unknown command: " ++ part1

parseLs :: [String] -> Cmd
parseLs list = ListDir $ map parseLsLine list

parseLsLine :: String -> Entry
parseLsLine str = let
    (p1, p2) = break (==' ') str
    in case p1 of
        "dir" -> Dir [tail p2]
        _     -> File [tail p2] (readNum p1)

parseCd :: String -> Cmd
parseCd str = let
    (_, p2) = break (==' ') str
    in ChangeDir (tail p2)


buildFilesystem :: [Cmd] -> Filesystem
buildFilesystem = (rootDir :) . snd . foldl processCmd ([], [])

processCmd :: (Path, Filesystem) -> Cmd -> (Path, Filesystem)
processCmd (currentDir, filesystem) cmd = case cmd of
    ListDir entries -> (currentDir, filesystem ++ map (prependPath currentDir) entries)
    ChangeDir dir -> case dir of
        ".." -> (init currentDir, filesystem)
        "/"  -> ([], filesystem)
        _    -> (currentDir ++ [dir], filesystem)


dirsSizes :: Filesystem -> [Int]
dirsSizes fs = map (entriesSize fs . entryPath) (filter isDir fs)

entriesSize :: Filesystem -> Path -> Int
entriesSize fs path = let
    children = filter ((== path) . take (length path) . entryPath) fs
    in sum $ map entrySize children

entrySize :: Entry -> Int
entrySize entry = case entry of
    File _ size -> size
    _           -> 0


part1 :: [Int] -> Int
part1 = sum . filter (<= 100000)

part2 :: [Int] -> Int
part2 sizes = let
    totalSpace = 70000000
    requiredFreeSpace = 30000000
    usedSpace = head sizes
    actualFreeSpace = totalSpace - usedSpace
    needToFree = requiredFreeSpace - actualFreeSpace
    in head $ dropWhile (< needToFree) $ sort sizes
