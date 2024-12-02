-- Stuff I might need in other tasks
module Util where
    import System.IO

    fir_last xs = case xs of
        x:xs -> [head x, last x] : fir_last xs
        otherwise -> []

    left xs = case xs of
        x:xs -> head x : left xs
        otherwise -> []

    right xs = case xs of
        x:xs -> last x : right xs
        otherwise -> []

    to_num xs = case xs of
        [] -> []
        x:xs -> (read x :: Int) : to_num xs

    split :: Char -> String -> [String]
    split c str = case str of
        [] -> []
        a:_ | a == c -> [""] ++ rest
        a:[] -> [a:""]
        a:as -> (a : head rest) : tail rest
        where
            rest = split c (tail str)

    -- For when you have an input with two columns
    get_cols str = fir_last $ map (split ' ') $ split '\n' str
