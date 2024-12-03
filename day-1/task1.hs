import Data.List
import Debug.Trace

sum_diffs :: [Int] -> [Int] -> Int
sum_diffs [] [] = trace "hej med dig" 0
sum_diffs (x:[]) (y:[]) = abs (x - y)
sum_diffs (x:xs) (y:ys) = abs (head xs - head ys) + sum_diffs (tail xs) (tail ys)

main :: IO ()
main = do
    print (sum_diffs (sort [3,4,2,1,3,3]) (sort [4,3,5,3,9,3]))
