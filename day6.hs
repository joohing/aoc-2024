import Data.List
import Data.Char
import qualified Data.Map as M
import System.IO
import Debug.Trace
import Control.Monad
import Util

data Guard = Guard Point Dir
data Point = Point Int Int deriving Eq
data Dir = FaceUp | FaceDown | FaceLeft | FaceRight deriving Eq
newtype AreaMap = AreaMap [String] deriving Show

instance Show Guard where
    show (Guard (Point x y) d) = "Guard((" ++ show x ++ ", " ++ show y ++ "), " ++ show d ++ ")"

instance Show Dir where
    show d = case d of
        FaceUp -> "FaceUp"
        FaceDown -> "FaceDown"
        FaceLeft -> "FaceLeft"
        FaceRight -> "FaceRight"

instance Show Point where
    show (Point x y) = "(x: " ++ show x ++ ", y: " ++ show y ++ ")"

-- Find and read the guard's state from the map. Assumes she's in start state ('^').
get_guard :: AreaMap -> Maybe Guard
get_guard am
    | null m = trace "No map given." Nothing
    | found /= -1 = Just $ Guard (Point found 0) FaceUp
    | otherwise = Just $ Guard (Point next_x (1 + next_y)) next_dir
    where
        AreaMap m = am
        found = idxof '^' (head m)
        Just (Guard (Point next_x next_y) next_dir) = get_guard $ AreaMap (tail m)

-- Is the point in bounds of the map?
p_in_bounds :: Point -> AreaMap -> Bool
p_in_bounds p am = (0 <= x && x < length m) && (0 <= y && y < (length $ head m))
    where
        Point x y = p
        AreaMap m = am

-- Get the char at point on map. Positive y is downwards
char_at_point :: Point -> AreaMap -> Maybe Char
char_at_point p am
    | null m = Nothing
    | otherwise = Just . head $ drop x $ head (drop y m)
    where
        Point x y = p
        AreaMap m = am

-- Find next collision point (obstacle) when moving in Dir from Point. Assumes the map is square.
-- If you got nothing, you left the map. Or gave it wrong parameters.
next_collision :: Point -> Dir -> AreaMap -> Maybe Point
next_collision p d am
    | not (p_in_bounds p am)            = trace "Point not in bounds, no collision." Nothing
    | char_at_point p am == Just '#'    = Just p
    | d == FaceLeft                     = next_collision (Point (x - 1) y) d am
    | d == FaceDown                     = next_collision (Point x (y + 1)) d am
    | d == FaceUp                       = next_collision (Point x (y - 1)) d am
    | d == FaceRight                    = next_collision (Point (x + 1) y) d am
    where
        AreaMap m = am
        Point x y = p

-- Return a new map with the given point painted over with an X.
draw_point :: Point -> AreaMap -> Maybe AreaMap
draw_point p am
    | null m = Nothing
    | not (p_in_bounds p am) = Nothing
    | otherwise = Just $ AreaMap (top ++ ([midleft ++ "X" ++ midright]) ++ bot)
    where
        AreaMap m = am
        Point x y = p
        top = take y m
        bot = drop (y + 1) m
        mid = take 1 $ drop y m
        midleft = take x $ head mid
        midright = drop (x + 1) $ head mid

-- Return a new map with a line of X's drawn between the two points on the given map.
draw_line :: Point -> Point -> AreaMap -> Maybe AreaMap
draw_line p1 p2 am
    | null m = Nothing
    | not (p_in_bounds p1 am) = Just am
    | p1 == p2 = draw_point p1 am
    | otherwise = draw_point p1 am >>= draw_line (Point (x1 + dx) (y1 + dy)) p2
    where
        AreaMap m = am
        Point x1 y1 = p1
        Point x2 y2 = p2
        dx = sign (x2 - x1)
        dy = sign (y2 - y1)

-- Get the place where the guard should be standing if she approaches an obstacle
-- at Point from Dir.
approach_obstacle :: Point -> Dir -> Point
approach_obstacle p d = case d of
    FaceUp      -> Point (x) (y + 1)
    FaceDown    -> Point (x) (y - 1)
    FaceRight   -> Point (x - 1) (y)
    FaceLeft    -> Point (x + 1) (y)
    where
        Point x y = p

-- Does what it says
rotate_right :: Dir -> Dir
rotate_right d = case d of
    FaceUp -> FaceRight
    FaceRight -> FaceDown
    FaceDown -> FaceLeft
    FaceLeft -> FaceUp

-- Get a point that's just off the edge in the direction the guard is walking.
find_edge :: Point -> Dir -> AreaMap -> Point
find_edge p d am = case d of
    FaceUp      -> Point x (-1)
    FaceDown    -> Point x (length m)
    FaceRight   -> Point (length $ head m) y
    FaceLeft    -> Point (-1) y
    where
        Point x y = p
        AreaMap m = am

-- Get a new tuple of (Guard, AreaMap) with new direction and position of the guard, plus
-- a new AreaMap with the guard's trail through the map drawn with X's.
walk_map :: Guard -> AreaMap -> Maybe (Guard, AreaMap)
walk_map g am
    | char_at_point curr_pos am == (Just '#') = trace "Guard inside obstacle!" Nothing
    | not (p_in_bounds curr_pos am) = Just (g, am)
    | otherwise = do
        next_pos <- if next_coll == Nothing then edge else next_coll
        next_map <- draw_line curr_pos next_pos am
        return (Guard next_pos (rotate_right curr_dir), next_map)
    where
        Guard curr_pos curr_dir = g
        coll = next_collision curr_pos curr_dir am
        next_coll = coll >>= (\c -> Just $ approach_obstacle c curr_dir)
        edge = Just $ find_edge curr_pos curr_dir am

-- Walk until the guard is off the mapped area.
keep_walking :: Guard -> AreaMap -> Maybe (Guard, AreaMap)
keep_walking g am
    | not $ p_in_bounds g_pos am = Just (g, am)
    | otherwise = walk_map g am >>= (\t -> keep_walking (fst t) (snd t))
    where
        Guard g_pos g_dir = g
        AreaMap m = am

task1 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle

    let lines = split '\n' contents
    let am = AreaMap lines
    let Just g = get_guard am
    let Just (Guard p d, AreaMap walked) = keep_walking g am

    print ("Task 1 result: " ++ show (sum $ map (count 'X') walked))

-- Fix the input before running task1 on it.
task2 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle

    print ("Task 2 result: not done")

main = do
    task1 "input/day6-input.txt"
    task2 "input/day6-sample.txt"
