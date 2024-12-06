import Data.List
import Data.Char
import System.IO
import Debug.Trace
import Control.Monad
import Util

-- We need to examine all 4x4 squares of the input. We can map the string using the win function from util.hs.
to_squares :: Int -> String -> [[String]]
to_squares n str = vert_wins n str >>= \sqr -> hor_wins n sqr

-- Transfrom a square sqr, e.g.
-- hor_wins 2 "012" -> "01"  "12"
--            "345"    "34", "45"
hor_wins :: Int -> [String] -> [[String]]
hor_wins n sqr = map (\idx -> map (take n) (map (drop idx) sqr)) $ [0..length (head sqr) - n]

vert_wins :: Int -> String -> [[String]]
vert_wins n str = win n (split '\n' str)

-- Returns how many vertical occurences of XMAS or SAMX it encounters in the given squares.
check_vert :: String -> [[String]] -> Int
check_vert str sqrs
    | null sqrs = 0
    | otherwise = count str cols + count (reverse str) cols
    where
        cols = map join $ join (map (hor_wins 1) sqrs)

task1 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle
    let sqrs = to_squares 3 contents
    print $ sqrs
    print $ check_vert "XMAS" sqrs
    print ("Task 1 result: not done")

task2 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle
    print ("Task 2 result: not done")

main = do
    task1 "input/day4-sample.txt"
    task2 "input/day4-sample.txt"
