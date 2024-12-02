import Data.List
import System.IO
import Util

sum_diffs :: [Int] -> [Int] -> Int
sum_diffs [] [] = 0
sum_diffs (x:xs) (y:ys) = abs (x - y) + sum_diffs xs ys

-- Do task1-calculations on file "input"
task1 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle
    let nums = get_cols contents
    let (l, r) = (to_num (left nums), to_num (right nums))
    print ("Task 1 result: " ++ show (sum_diffs (sort l) (sort r)))

count :: Int -> [Int] -> Int
count x ys = case ys of
    [] -> 0
    (y:ys) -> if x == y then 1 + count x ys else count x ys

sim_score :: ([Int], [Int]) -> Int
sim_score (xs, ys) = case xs of
    [] -> 0
    (x:xs) -> x * count x ys + sim_score (xs, ys)

task2 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle
    let nums = get_cols contents
    let (l, r) = (to_num (left nums), to_num (right nums))
    print $ "Task 2 result: " ++ show (sim_score (l, r))

main = do
    task1 "input/day1-input.txt"
    task2 "input/day1-sample.txt"
