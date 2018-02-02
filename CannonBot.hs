module CannonBot where

import Data.Char
import Util

getMove :: String -> String
listMoves :: String -> String

--data Player = Player{row::Int, col::Int, white::Bool}
data City = City {row :: Int, col :: Int, white :: Bool}
--data Field = Player | City | Empty

instance Show City where
    show (City r c True) = "W{r:" ++ show r ++ ",c:" ++ show c ++ "}"
    show (City r c False) = "B{r:" ++ show r ++ ",c:" ++ show c ++ "}"

--getWhitePlayers :: String -> [Player]
--getBlackPlayers :: String -> [Player]

stringToCity :: String -> Int -> City
stringToCity (l:t) c = let w = c == 0 in (City ((digitToInt l) + 1) c w)

getCities :: String -> [City]
getCities fen = let l = (splitOn "/" fen) in (stringToCity (head l) 0):(stringToCity (last l) 10):[]
 
getWhiteCity :: [Char] -> City
getWhiteCity fen = City (digitToInt (head (head (splitOn "/" fen)))) 0 False

--getBlackCity :: String -> City

-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: a0-b0
getMove a = "a0-b0"

-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: [a0-b0,a0-b0]
listMoves a = "[a0-b0,a0-b0]"
