module CannonBot where

import Data.Char
import Util

getMove :: String -> String
listMoves :: String -> String

-- UTILITIES

-- Char representation of column number
nToC :: Int -> Char
nToC i = ['a'..'j']!!i

-- Formatting positions
iToP :: Int -> (Int,Int)
iToP i = ((rem i 10), (quot i 10))

-- postition to position char pair (column, row)
iToPc :: Int -> (Char,Int)
iToPc i = ((nToC ((rem i 10)), [9,8..0]!!(quot i 10)))

-- position char pair (col,row) to string colrow
pcToC :: (Char,Int) -> String
pcToC (c,r) = [c] ++ [(intToDigit r)]

-- format move to string src-dst
formatMove :: Int -> Int -> String
formatMove c t = pcToC(iToPc c) ++ "-" ++ pcToC(iToPc t)

-- row of position
pToR :: Int -> Int
pToR i = quot i 10

-- column of position
pToC :: Int -> Int
pToC i = rem i 10

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
enemy _ = 'x'

findAfter :: (Char -> Bool) -> String -> Int -> Int
findAfter f (h:[]) n = if f h then n else -1
findAfter f (h:r) n = if f h then n else (findAfter (f) r (n + 1))

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

-- Join List of Strings into one String
stringsToListString :: [String] -> String
stringsToListString ([]) = []
stringsToListString (h:r) = h ++ "," ++ (stringsToListString r)

-- Find all indices of Char in String
findIndices :: String -> Char -> [Int]
findIndices s c = snd (unzip (filter (\p -> (fst p) == c) (zip s [0..])))

-- Offsets
moveNextRowOffset :: [Int]
moveNextRowOffset = [9..11]

beatOffset :: [Int]
beatOffset = [-1,1] ++ moveNextRowOffset

retreatOffset :: [Int]
retreatOffset = [-18,-20,-22]

-- PARSE FIELD

-- Return whose turn it is
getPlayerTurn :: String -> Char
getPlayerTurn s = last s

-- Cut players turn indicator from FEN
-- TODO: fix if no turn indicator was passed
getRowsOnly :: String -> String
getRowsOnly s = head (splitOn " " s)

-- Turn FEN String into list of strings (one per row)
getRows :: String -> [String]
getRows s = splitOn "/" s

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

-- List of allies indices
allies :: String -> [Int]
allies s = let fs = fieldString s in findIndices fs (getPlayerTurn s)

-- homebase index
homebase :: String -> [Int]
homebase s = let fs = fieldString s in findIndices fs (toUpper (getPlayerTurn s))

-- list of enemies indices
enemies :: String -> [Int]
enemies s = let fs = fieldString s in findIndices fs (enemy (getPlayerTurn s))

-- enemy base index
enemyBase :: String -> [Int]
enemyBase s = let fs = fieldString s in findIndices fs (toUpper (enemy (getPlayerTurn s)))

-- RULES    

-- Target field free
targetFieldFree :: String -> Int -> Bool
targetFieldFree s i = (fieldString s)!!i == '1'

-- Path does not cross field borders
pathNotOutOfBounds :: Int -> Int -> Bool
pathNotOutOfBounds src dst = ((mod src 10) - (mod dst 10) `elem` [-1,0..1]) && (elem dst [0..99])

-- Target field occupied by enemy
targetFieldOccupiedByEnemy :: String -> Int -> Char -> Bool
targetFieldOccupiedByEnemy s i c = (fieldString s)!!i == (enemy c)

-- Player threated
playerIsThreatened :: String -> Int -> Bool
playerIsThreatened s i = let mf = (map (\f -> (i + f)) beatOffset) in (foldr (||) False (map (\f -> targetFieldOccupiedByEnemy s f (s!!i)) mf))

-- Fields a player can move to by beating a potential enemy
fieldsPlayerCanMoveToWithBeating :: String -> Int -> [(Int,Bool)]
fieldsPlayerCanMoveToWithBeating s i = filter (\f -> snd f) (let bf = (map (+i) beatOffset); mf = (map (+i) moveNextRowOffset) in (zip bf (map (\f -> (pathNotOutOfBounds i f) && ((targetFieldFree s f) && (elem f mf)) || (targetFieldOccupiedByEnemy s f (s!!i))) bf)))

fieldsPlayerCanRetreatTo :: String -> Int -> [(Int, Bool)]
fieldsPlayerCanRetreatTo s i = if (playerIsThreatened s i) then filter (\f -> snd f) (let mf = (map (+i) retreatOffset) in (zip mf (map (\f -> (targetFieldFree s f)) mf))) else []

-- Wrapper for fieldsPlayerCanMoveToWithBeating
fieldsPlayerCanMoveTo :: String -> Int -> [Int]
fieldsPlayerCanMoveTo s i = fst (unzip ((fieldsPlayerCanMoveToWithBeating s i) ++ (fieldsPlayerCanRetreatTo s i)))

-- List Moves a Player can make
playerCanMakeMoves :: String -> Int -> [String]
playerCanMakeMoves s i = map(\t -> (formatMove i t)) (fieldsPlayerCanMoveTo s i)

printMoves :: String -> [String]
printMoves s = (map (\a -> (stringsToString (playerCanMakeMoves s a))) (allies s))



-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: a0-b0
getMove a = "a0-b0"

-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: [a0-b0,a0-b0]
--listMoves a = "[a0-b0,a0-b0]"
listMoves s = stringsToListString (map (\a -> (stringsToString (playerCanMakeMoves s a))) (allies s))
