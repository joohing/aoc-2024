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
--            "012"    "01"  "12"
-- hor_wins 2 "345" -> "34", "45"
hor_wins :: Int -> [String] -> [[String]]
hor_wins n sqr = map (\idx -> map (take n) (map (drop idx) sqr)) $ [0..length (head sqr) - n]

vert_wins :: Int -> String -> [[String]]
vert_wins n str = win n (split '\n' str)

-- Returns how many vertical occurences of str or reverse str it encounters in the given lines.
check_vert :: String -> [String] -> Int
check_vert str lines
    | null lines = 0
    | otherwise = (count str cols) + (count (reverse str) cols)
    where
        cols = map join $ hor_wins 1 lines

-- Grab the diag starting from top left and down to the bottom right
to_diag :: [String] -> [String]
to_diag sqr | null sqr = []
            | otherwise = (map (take 1) $ take 1 sqr) ++ (to_diag $ (map (drop 1) (drop 1 sqr)))

-- To get down-up diags: flip order of each line in each square.
-- "12", "34" -> "34, "12"
task1 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle

    -- Diags.
    let squares = to_squares 4 contents
    let down_up_diags = map join $ map to_diag (map reverse squares)
    let top_down_diags = map join $ map to_diag squares

    -- Rows and columns. hor_wins 4 on both, because that makes checking for string equality easier. Performance is just a scam anyways invented by big fast to sell more quick
    let lines = split '\n' contents
    let rows = hor_wins 4 $ lines
    let cols = hor_wins 4 (map join $ hor_wins 1 lines)

    let xmas_count = count "XMAS" (down_up_diags ++ top_down_diags ++ join rows ++ join cols)
    let xmas_rev_count = count (reverse "XMAS") (down_up_diags ++ top_down_diags ++ join rows ++ join cols)

    print ("Task 1 result: " ++ show (xmas_count + xmas_rev_count))

-- For this one, we only need the diags, and then check for MAS or SAM instead of XMAS.
task2 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle

    -- We can get an X by concatting the down_up diag with the up_down diag.
    let squares = to_squares 3 contents
    let down_up_diags = map join $ map to_diag (map reverse squares)
    let top_down_diags = map join $ map to_diag squares
    let xs = zip down_up_diags top_down_diags
    let pred = (\t -> (fst t == "SAM" || fst t == "MAS") && (snd t == "SAM" || snd t == "MAS"))
    let xmas = filter pred xs

    let mas_count = length xmas

    print ("Task 2 result: " ++ show mas_count)

main = do
    task1 "input/day4-input.txt"
    task2 "input/day4-input.txt"
