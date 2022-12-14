module Day13 where

import Common (Solution(Solution), NoSolution(..), readNum)
import Control.Monad.State (State, MonadState(state, get), gets, modify, evalState)
import Data.Maybe ( fromJust )
import Data.List ( uncons, sort )
import Data.List.Split ( splitOn )
import Debug.Trace (traceShow)
import GHC.IO.Handle (Newline(LF))

solution = Solution "day13" "" run

run input = let
    pairs = parse input
    in (part1 pairs, part2 pairs)

parse :: String -> [(Json, Json)]
parse = map parsePair . filter (/="") . splitOn "\n\n"

parsePair :: String -> (Json, Json)
parsePair str = let
    [p1, p2] = lines str
    in (evalState parseJson p1, evalState parseJson p2)


-- A little handmade JSON parser. Why not? :)

type ParseState = State String
data Json = JsonList [Json] | JsonValue Int  deriving (Show, Eq)

parseJson :: ParseState Json
parseJson = do
    char <- gets head
    case char of
        '[' -> parseList
        _   -> parseValue

isBreakCharacter :: Char -> Bool
isBreakCharacter = (`elem` "],")

parseValue :: ParseState Json
parseValue = do
    value <- state $ break isBreakCharacter
    return $ JsonValue (readNum value)


parseList :: ParseState Json
parseList = do
    modify tail  -- drop [
    listElems <- parseListElems
    modify tail  -- drop ]
    return $ JsonList listElems

parseListElems :: ParseState [Json]
parseListElems = do
    char <- gets head
    case char of
        ']' -> return []
        ',' -> modify tail >> parseListElems
        _   -> do
            listElem <- parseJson
            rest <- parseListElems
            return $ listElem : rest

readChar :: ParseState Char
readChar = state $ fromJust . uncons


instance Ord Json where
    compare val1 val2 = case (val1, val2) of
        (JsonValue a, JsonValue b)         -> compare a b
        (a@(JsonList _), b@(JsonValue _))  -> compare a (JsonList [b])
        (a@(JsonValue _), b@(JsonList _))  -> compare (JsonList [a]) b
        (JsonList a, JsonList b)           -> compareLists a b


compareLists :: [Json] -> [Json] -> Ordering
compareLists [] [] = EQ
compareLists [] _  = LT
compareLists _ []  = GT
compareLists (a:aRest) (b:bRest) = case compare a b of
                                        EQ -> compareLists aRest bRest
                                        v  -> v

part1 :: [(Json, Json)] -> Int
part1 = sum . map fst . filter snd . zip [1..] . map (uncurry (<))


part2 :: [(Json, Json)] -> Int
part2 = product                                                        -- 3. multiply everything
      . map fst . filter ((`elem` dividerPackets) . snd) . zip [1..]   -- 2. filter divider packets with its positions 
      . sort . (++ dividerPackets) . concatMap (\(a, b) -> [a, b])     -- 1. sort everything with divider packets 

dividerPackets :: [Json]
dividerPackets = [JsonList [JsonList [JsonValue 2]], JsonList [JsonList [JsonValue 6]]]
