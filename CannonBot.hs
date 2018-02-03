module CannonBot where

import Data.Char
import Util

getMove :: String -> String
listMoves :: String -> String

data Player = Player {pc :: Int, pr :: Int, pw :: Bool}
data City = City {cc :: Int, cr :: Int, cw :: Bool}

instance Show City where
    show (City cc cr True) = "W{" ++ id (coordsString cc cr) ++ "}"
    show (City cc cr False) = "B{" ++ id (coordsString cc cr) ++ "}"

instance Show Player where
    show (Player pr pc True) = "w{" ++ id (coordsString pr pc) ++ "}"
    show (Player pr pc False) = "b{" ++ id (coordsString pr pc) ++ "}"

-- Utilities
coordsString :: Int -> Int -> String
coordsString c r = (colNumToChar c) : intToDigit r : []

colNumToChar :: Int -> Char
colNumToChar i = "abcdefghij"!!i

isPlayer :: Char -> Bool
isPlayer c = elem c "wb"

findAfter :: (Char -> Bool) -> String -> Int -> Int
findAfter f (h:[]) n = if f h then n else -1
findAfter f (h:r) n = if f h then n else (findAfter (f) r (n + 1))

findFirstPlayer :: String -> Int
findFirstPlayer s = (findAfter isPlayer s 0)

-- Logic

-- TODO: Fix 4W5 vs 1111W5 Format issue
-- TODO: Fix row 0 -> White issue (?)
-- 4W5
stringToCity :: String -> Int -> City
stringToCity (l:t) c = let w = c == 0 in (City ((digitToInt l) + 1) c w)

-- 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2
getCitiesList :: String -> [City]
getCitiesList fen = let (lh:lr) = (splitOn "/" fen) in (stringToCity lh 9):(stringToCity (last lr) 0) : []

getPlayerStrings :: String -> [String]
getPlayerStrings fen = let (lh:lr) = (splitOn "/" fen) in take 8 lr

-- w 1w1w1w1w1w
findIndexPlayers :: Char -> String -> [String]
findIndexPlayers c row = (splitOn [c] row)

-- getPlayerOffsets :: [Char] -> [Int] -> [Int]
-- getPlayerOffsets ('':r) [] = [0] ++ getPlayerOffsets r 1
-- getPlayerOffsets (h:'':[]) l = l
-- getPlayerOffsets (h:r) l = l ++ [(last l) + h]

-- 1w1w1w1w1w Row
--getPlayers :: String -> Int -> [Player]
--getPlayers row rc = 



-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: a0-b0
getMove a = "a0-b0"

-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: [a0-b0,a0-b0]
listMoves a = "[a0-b0,a0-b0]"
