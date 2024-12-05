import Data.List
import Data.Char
import System.IO
import Debug.Trace
import Util

-- I solve this task by modelling a "state machine" of sorts that passes the string around to different states.
-- The first is "match_mul" which tries to find 'mul(d[dd], d[dd])' instructions
match_mul :: String -> [(Int, Int)]
match_mul str
    | "mul(" == take 4 str = (match_nums $ take 8 (drop 4 str)) ++ (match_mul (drop 1 str))
    | null str = []
    | otherwise = match_mul (tail str)

-- Next up is where we see whether the paren on the other side is legit, and
-- send back nums if so
match_nums :: String -> [(Int, Int)]
match_nums str
    | valid = (rint fst, rint snd):[]
    | otherwise = []
    where
        fst = get_till ',' str
        snd = get_till ')' $ drop 1 (drop_till ',' str)
        valid = (all isDigit $ fst ++ snd)
                && elem ')' str && elem ',' str
                && 1 <= length fst && length fst <= 3
                && 1 <= length snd && length snd <= 3

-- Iter through tuples and sum them up
mul :: [(Int, Int)] -> Int
mul [] = 0
mul (x:xs) = fst x * snd x + mul xs

-- wrap match_mul and remove everything between a don't and a do
do_loop :: String -> [(Int, Int)]
do_loop str
    | take 7 str == "don't()" = do_loop $ get_after "do()" str
    | take 4 str == "mul(" = (match_mul $ take 12 str) ++ (do_loop $ drop 4 str)
    | null str = []
    | otherwise = do_loop $ drop 1 str

task1 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle
    let tpls = match_mul contents
    print ("Task 1 result: " ++ (show $ mul tpls))

task2 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle
    let tpls = do_loop contents
    print ("Task 2 result: " ++ (show $ mul tpls))

main = do
    task1 "input/day3-input.txt"
    task2 "input/day3-input.txt"
