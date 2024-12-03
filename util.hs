-- Stuff I might need in other tasks
module Util where

import System.IO
import Data.List

-- Get the first and last string in a list of list of strings
fir_last :: [[String]] -> [[String]]
fir_last xs = case xs of
    x:xs -> [head x, last x] : fir_last xs
    otherwise -> []

non_empty :: [[String]] -> [[String]]
non_empty xs = filter (\x -> x /= [""]) xs

left :: [[a]] -> [a]
left xs = case xs of
    x:xs -> head x : left xs
    otherwise -> []

right :: [[a]] -> [a]
right xs = case xs of
    x:xs -> last x : right xs
    otherwise -> []

to_num :: [String] -> [Int]
to_num xs = case xs of
    [] -> []
    x:xs -> (read x :: Int) : to_num xs

sign :: Int -> Int
sign x
    | x < 0 = -1
    | x > 0 = 1
    | x == 0 = 0

-- Print all in a list with newlines between
prall xs = putStr $ intercalate "\n" (map show xs) ++ "\n"

-- Count of x in ys
count :: (Eq a) => a -> [a] -> Int
count x ys = case ys of
    [] -> 0
    (y:ys) -> if x == y then 1 + count x ys else count x ys

split :: Char -> String -> [String]
split c str = case str of
    [] -> []
    a:_ | a == c -> [""] ++ rest
    a:[] -> [a:""]
    a:as -> (a : head rest) : tail rest
    where
        rest = split c (tail str)

-- For when you have an input with two columns
get_fir_last str = fir_last $ map (split ' ') $ split '\n' str

-- For when you have an input with any count of columns
get_cols str = non_empty $ map (split ' ') $ split '\n' str

-- Sliding window of size n
win :: Int -> [Int] -> [[Int]]
win n xs = case xs of
    [] -> []
    otherwise -> take n xs : win n (tail xs)

-- Map to runs of unique elements, i.e. [1, 1, 2, 1, 3, 4] becomes [1, 2, 1, 3, 4]
to_run :: (Eq a) => a -> [a] -> [a]
to_run p xs = case xs of
    [] -> []
    x:xs -> if x == p then to_run p xs else x:(to_run x xs)
