import Data.List
import System.IO
import Debug.Trace
import Util

valid_ranges :: [Int] -> Bool
valid_ranges xs = case xs of
    [] -> True
    x:[] -> True
    x:y:xs -> 1 <= (abs $ x - y)
           && (abs $ x - y) <= 3
           && valid_ranges (y:xs)

-- Do the numbers move in one direction in all steps?
same_dir :: [Int] -> Bool
same_dir xs = case xs of
    [] -> True
    x:[] -> True
    x:y:[] -> True
    x:y:z:xs -> (sign (x - y) == sign (y - z)) && (same_dir $ [y, z] ++ xs)

-- Use the two functions above to check if the given int list is safe
is_safe :: [Int] -> Bool
is_safe xs = same_dir xs && valid_ranges xs

-- Fixable if any subset of the list with one element removed is safe
fixable :: [Int] -> Bool
fixable xs = any is_safe (subsets (length xs - 1) xs)

task1 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle
    let nums = map to_num $ get_cols ' ' contents
    let result = map is_safe nums
    print ("Task 1 result: " ++ show (count True result))

task2 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle
    let nums = map to_num $ get_cols ' ' contents
    let result = map fixable nums
    print ("Task 2 result: " ++ show (count True result))

main = do
    task1 "input/day2-input.txt"
    task2 "input/day2-input.txt"
