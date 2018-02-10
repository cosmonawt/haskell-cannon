module CannonBot where

import Data.Char
import Util

getMove :: String -> String
listMoves :: String -> String

-- --------------------------------------------------------
-- UTILITIES
-- --------------------------------------------------------

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

-- Offsets based on player
moveNextRowOffset :: [Int]
moveNextRowOffset= [9..11]
--moveNextRowOffset 'b' = [-11..-9]

beatOffset :: [Int]
beatOffset = [-1,1] ++ (moveNextRowOffset)

retreatOffset :: [Int]
retreatOffset = [-18,-20,-22]

baseOffset :: Char -> [Int]
baseOffset 'w' = [1..8]
baseOffset 'b' = [91..98]

vertFwdCannonOffset :: [Int]
vertFwdCannonOffset = [10,20]

vertBwdCannonOffset :: [Int]
vertBwdCannonOffset = [-10,-20]

horiCannonOffset :: [Int]
horiCannonOffset = [1,2]

diagRightCannonOffset :: [Int]
diagRightCannonOffset = [9,18]

diagLeftCannonOffset :: [Int]
diagLeftCannonOffset = [11,22]

-- --------------------------------------------------------
-- PARSE FIELD
-- --------------------------------------------------------

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

-- base of current player is missing
baseMissing :: String -> Bool
baseMissing s = (length (homebase s)) == 0

-- --------------------------------------------------------
-- RULES
-- --------------------------------------------------------

-- Set Base
setBase :: String -> [String]
setBase s = map(\t -> (formatMove t t)) (baseOffset (getPlayerTurn s))

-- Target field free
targetFieldFree :: String -> Int -> Bool
targetFieldFree s i = (fieldString s)!!i == '1'

-- Path does not cross field borders
pathNotOutOfBounds :: Int -> Int -> Bool
pathNotOutOfBounds src dst = ((mod src 10) - (mod dst 10) `elem` [-1,0..1]) && (elem dst [0..99])

-- Target field occupied by enemy
targetFieldOccupiedByEnemy :: String -> Int -> Char -> Bool
targetFieldOccupiedByEnemy s i c = s!!i == (enemy c)

-- Field occupied by ally
targetFieldOccupiedByAlly :: String -> Int -> Char -> Bool
targetFieldOccupiedByAlly s i c = s!!i == c

-- Player threated
playerIsThreatened :: String -> Int -> Bool
playerIsThreatened s i = let mf = (map (\f -> (i + f)) beatOffset) 
    in (foldr (||) False (map (\f -> targetFieldOccupiedByEnemy s f (s!!i)) mf))

-- Fields a player can move to by beating a potential enemy
fieldsPlayerCanMoveToWithBeating :: String -> Int -> [(Int,Bool)]
fieldsPlayerCanMoveToWithBeating s i = filter (\f -> snd f) (beatableFields s i)

playerMovement :: Char -> Int -> (Int -> Int)
playerMovement 'w' i = (+i)
playerMovement 'b' i = (-)i

beatableFields :: String -> Int -> [(Int,Bool)]
beatableFields s i = let bf = map (playerMovement (s!!i) i) beatOffset; mf = map (playerMovement (s!!i) i) moveNextRowOffset
    in zip bf (map (\f -> (pathNotOutOfBounds i f) && ((targetFieldFree s f) && (elem f mf)) || (targetFieldOccupiedByEnemy s f (s!!i))) bf)

fieldsPlayerCanRetreatTo :: String -> Int -> [(Int, Bool)]
fieldsPlayerCanRetreatTo s i = if (playerIsThreatened s i) 
    then filter (\f -> snd f) (let mf = (map (playerMovement (s!!i) i) retreatOffset) 
        in (zip mf (map (\f -> (targetFieldFree s f)) mf))) 
    else []

-- Wrapper for fieldsPlayerCanMoveToWithBeating
fieldsPlayerCanMoveTo :: String -> Int -> [Int]
fieldsPlayerCanMoveTo s i = fst (unzip ((fieldsPlayerCanMoveToWithBeating s i) ++ (fieldsPlayerCanRetreatTo s i)))

-- --------------------------------------------------------
-- Cannons
-- --------------------------------------------------------

-- Player End of vertical cannon
playerIsPartOfVertFwdtCannon :: String -> Int -> Bool
playerIsPartOfVertFwdtCannon s i = foldl (&&) True (map (\f -> (targetFieldOccupiedByAlly s (i+f) (s!!i))) vertFwdCannonOffset)

playerIsPartOfVertBwdtCannon :: String -> Int -> Bool
playerIsPartOfVertBwdtCannon s i = foldl (&&) True (map (\f -> (targetFieldOccupiedByAlly s (i+f) (s!!i))) vertBwdCannonOffset)

-- Player End of horizontal cannon
playerIsPartOfHoriLeftCannon :: String -> Int -> Bool
playerIsPartOfHoriLeftCannon s i = foldl (&&) True (map (\f -> (targetFieldOccupiedByAlly s (i+f) (s!!i))) horiCannonOffset)

playerIsPartOfHoriRightCannon :: String -> Int -> Bool
playerIsPartOfHoriRightCannon s i = foldl (&&) True (map (\f -> (targetFieldOccupiedByAlly s (i-f) (s!!i))) horiCannonOffset)

-- Player End of diagonal cannon
playerIsPartOfDiagLeftCannon :: String -> Int -> Bool
playerIsPartOfDiagLeftCannon s i = foldl (&&) True (map (\f -> (targetFieldOccupiedByAlly s (i+f) (s!!i))) diagLeftCannonOffset)

playerIsPartOfDiagRightCannon :: String -> Int -> Bool
playerIsPartOfDiagRightCannon s i = foldl (&&) True (map (\f -> (targetFieldOccupiedByAlly s (i+f) (s!!i))) diagRightCannonOffset)

-- Moves

playerCannonMoves :: String -> Int -> [Int] -> [String]
playerCannonMoves s i list = (map (\f -> (let x = i+f in (if (targetFieldOccupiedByEnemy s x (s!!i)) then (formatMove i x) else ""))) list)

playerCannonVertFwdMoves :: String -> Int -> [String]
playerCannonVertFwdMoves s i = playerCannonMoves s i [40,50]

playerCannonVertBwdMoves :: String -> Int -> [String]
playerCannonVertBwdMoves s i = playerCannonMoves s i [-40,-50]

playerCannonHoriRightMoves :: String -> Int -> [String]
playerCannonHoriRightMoves s i = playerCannonMoves s i [4,5]

playerCannonHoriLeftMoves :: String -> Int -> [String]
playerCannonHoriLeftMoves s i = playerCannonMoves s i [-4,-5]

playerCannonDiagFRightMoves :: String -> Int -> [String]
playerCannonDiagFRightMoves s i = playerCannonMoves s i [36,45]

playerCannonDiagFLeftMoves :: String -> Int -> [String]
playerCannonDiagFLeftMoves s i = playerCannonMoves s i [44,55]

playerCannonDiagBRightMoves :: String -> Int -> [String]
playerCannonDiagBRightMoves s i = playerCannonMoves s i [-36,-45]

playerCannonDiagBLeftMoves :: String -> Int -> [String]
playerCannonDiagBLeftMoves s i = playerCannonMoves s i [-44,-55]



-- --------------------------------------------------------
-- Execute
-- --------------------------------------------------------

-- List Moves a Player can make
playerCanMakeMoves :: String -> Int -> [String]
playerCanMakeMoves s i = map(\t -> (formatMove i t)) (fieldsPlayerCanMoveTo (fieldString s) i)

printMoves :: String -> [String]
printMoves s = if (baseMissing s) then (setBase s) else (map (\a -> (stringsToString (playerCanMakeMoves s a))) (allies s))



-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: a0-b0
getMove a = "a0-b0"

-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: [a0-b0,a0-b0]
--listMoves a = "[a0-b0,a0-b0]"
listMoves s = stringsToListString (map (\a -> (stringsToString (playerCanMakeMoves s a))) (allies s))
