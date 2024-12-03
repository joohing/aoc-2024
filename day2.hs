import Data.List
import System.IO
import Debug.Trace
import Util

-- A problem has an int which is the position in the list.
data Prb pos = Dir Int | Diff Int

-- A Dir problem can be fixed if the neighbours are >1 apart.
-- A Diff problem can be fixed if the neighbours are <= 6 apart.
fix prb@(Prb pos) xs = case xs of
    [] -> False
    x:[] -> False
    x:y:[] -> False
    x:y:z:xs -> if pos == 1
                then (fix_prb (x, y, z) prb)
                else (fix )

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

-- Fixable if count of non-adjacent slices of length 2 is <= 1
fixable :: [Int] -> Bool
fixable xs = (count False $ to_run True $ map (\x -> is_safe x) (win 3 xs)) <= 1

task1 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle
    let nums = map to_num $ get_cols contents
    let result = map is_safe nums
    -- prall result
    print ("Task 1 result: " ++ show (count True result))

task2 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle
    let nums = map to_num $ get_cols contents
    let result = map fixable nums
    print "prall result:"
    prall result
    print ("Task 2 result: " ++ show (count True result))

main = do
    task1 "input/day2-input.txt"
    task2 "input/day2-sample.txt"
