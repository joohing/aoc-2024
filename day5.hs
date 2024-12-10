import Data.List
import Data.Char
import qualified Data.Map as M
import System.IO
import Debug.Trace
import Control.Monad
import Util

-- A line is valid if it satisfies all rules in the map.
-- Reverse the list, and check that any item found in the rule must NOT be found to the right in the
-- reversed list.
find_probs :: M.Map String [String] -> [String] -> [(String, Int)]
find_probs rules l =
    concatMap (\p_idx -> check_rest (drop (snd p_idx) rev) $ lookup (fst p_idx))
        (zip rev [0..])
    where
        rev = reverse l
        check_rest line ps = map (\p -> (head line, idxof p (reverse line))) ps
        lookup p = maybe [] id $ M.lookup p rules

-- Find valid lines by asserting that there should be no problems.
valid_line :: M.Map String [String] -> [String] -> Bool
valid_line mp strs = if (all (\t -> (snd t) == -1) (find_probs mp strs))
                     then True
                     else False

-- Combine list found at key.
combine_insert :: (Ord k) => k -> [a] -> [a] -> [a]
combine_insert key new_value old_value = new_value ++ old_value

-- Collapse tuples with duplicates into a map from the left value to a list of the right values.
collapse :: (Ord a) => [(a, a)] -> M.Map a [a] -> M.Map a [a]
collapse strs mp
    | null strs = mp
    | otherwise = collapse (tail strs) next_map
    where
        next_map = M.insertWithKey combine_insert (fst $ head strs) ([snd $ head strs]) mp

-- While the line is not valid, keep swapping indeces.
fix_line :: M.Map String [String] -> [String] -> [String]
fix_line _ [] = []
fix_line mp l = if null probs
                then l
                else fix_line mp swapped
    where
        probs = filter (\t -> snd t /= -1) (find_probs mp l)
        swapped = swap (snd $ head probs) (idxof (fst $ head probs) l) l

-- A number maps to a set of numbers that must succeed it if present.
-- We then search backwards to see if any of those numbers preceed the number, in which case the rule is broken.
task1 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle

    let rule_tuples = map (\l -> (head l, last l)) $ takeWhile (\l -> not (null l)) $ get_cols '|' contents
    let rules = collapse rule_tuples M.empty
    let datas = map (split ',') $ drop 1 $ dropWhile (/= []) $ lines contents

    let valids = filter (valid_line rules) datas
    let middles = map (\l -> l !! div (length l) 2) valids

    let result = sum $ map rint middles

    print ("Task 1 result: " ++ show result)

-- Fix the input before running task1 on it.
task2 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle

    let rule_tuples = map (\l -> (head l, last l)) $ takeWhile (\l -> not (null l)) $ get_cols '|' contents
    let rules = collapse rule_tuples M.empty
    let datas = map (split ',') $ drop 1 $ dropWhile (/= []) $ lines contents

    let bad_lines = filter (not . valid_line rules) datas

    let fixed_lines = map (fix_line rules) bad_lines
    let middles = map (\l -> l !! div (length l) 2) fixed_lines

    let result = sum $ map rint middles

    print ("Task 2 result: " ++ show result)

main = do
    task1 "input/day5-input.txt"
    task2 "input/day5-input.txt"
