module CannonBot where

import Data.Char
import Util

getMove :: String -> String
listMoves :: String -> String

data Field = BlackPlayer | WhitePlayer | BlackCity | WhiteCity | Empty


-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: a0-b0
getMove a = "a0-b0"

-- Input Format: 4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w
-- Output Format: [a0-b0,a0-b0]
listMoves a = "[a0-b0,a0-b0]"
