module CannonBot where

import Data.Char
import Util

getMove :: String -> String
listMoves :: String -> String

-- UTILITIES

-- Char representation of column number
colNumToChar :: Int -> Char
colNumToChar i = ['a'..'j']!!i

-- Numbers [2..9] as chars
numbers29 :: String
numbers29 = ['2'..'9']

ones :: [Char]
ones = ['1','1'..]

isPlayer :: Char -> Bool
isPlayer c = elem c "wb"

isCity :: Char -> Bool
isCity c = elem c "WB"

enemy :: Char -> Char
enemy 'w' = 'b'
enemy 'b' = 'w'

findAfter :: (Char -> Bool) -> String -> Int -> Int
findAfter f (h:[]) n = if f h then n else -1
findAfter f (h:r) n = if f h then n else (findAfter (f) r (n + 1))

findFirstPlayer :: String -> Int
findFirstPlayer s = (findAfter isPlayer s 0)

-- Sum up numerical chars in list of chars
charSum :: [Char] -> Int
charSum s = foldr (+) 0 (map digitToInt s)

-- Replace chars with numerical >1 with list of numbers of equal sum
convertToNumbers :: Char -> [Char]
convertToNumbers c = if (elem c numbers29) then (take (digitToInt c) ones) else [c]

-- Join List of Strings into one String
stringsToString :: [String] -> String
stringsToString ([]) = []
stringsToString (h:r) = h ++ stringsToString r

-- Find all indices of Char in String
findIndices :: String -> Char -> [Int]
findIndices s c = snd (unzip (filter (\p -> (fst p) == c) (zip s [0..])))

-- PARSE FIELD

-- Return whose turn it is
getPlayerTurn :: String -> Char
getPlayerTurn s = last s

-- Cut players turn indicator from FEN
getRowsOnly :: String -> String
getRowsOnly s = head (splitOn " " s)

-- Turn FEN String into list of strings (one per row)
getRows :: String -> [String]
getRows s = splitOn "/" s

-- Split list of row strings into list of list of strings (["ab","cd"] -> [["","a","b"]["","c","d"]])
splitRows :: [String] -> [[String]]
splitRows ls = map (splitOn "") ls

-- Expand line with numbers >1 into line containing only 1s (1w3 -> 1w111 and "" -> 1..)
expandLine :: [Char] -> String
expandLine "" = take 10 ones
expandLine s = stringsToString (map convertToNumbers s)

-- Turn a FEN String into List of expanded rows
getField :: String -> [String]
getField s = map expandLine (getRows (getRowsOnly s))

-- Converts FEN into completely expanded string without "/"
fieldString :: String -> String
fieldString s = stringsToString (getField s)

allies :: String -> [Int]
allies s = let fs = fieldString s in findIndices fs (getPlayerTurn s)

enemies :: String -> [Int]
enemies s = let fs = fieldString s in findIndices fs (enemy (getPlayerTurn s))

-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: a0-b0
getMove a = "a0-b0"

-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: [a0-b0,a0-b0]
listMoves a = "[a0-b0,a0-b0]"
