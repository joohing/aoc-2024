import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import System.IO
import Debug.Trace
import Control.Monad
import Util

-- Guard (Position, Walking direction, Is looping)
data Guard = Guard Point Dir Bool
data Point = Point Int Int deriving Eq
data Dir = FaceUp | FaceDown | FaceLeft | FaceRight deriving Eq
newtype AreaMap = AreaMap [String]

dir_chars = "^v<>"

instance Show Guard where
    show (Guard (Point x y) d looping) =
        "Guard((" ++ show x ++ ", " ++ show y ++ "), " ++ show d ++ ", " ++ show looping ++ ")"

instance Show Dir where
    show d = case d of
        FaceUp -> "FaceUp"
        FaceDown -> "FaceDown"
        FaceLeft -> "FaceLeft"
        FaceRight -> "FaceRight"

instance Show Point where
    show (Point x y) = "(x: " ++ show x ++ ", y: " ++ show y ++ ")"

instance Show AreaMap where
    show (AreaMap am) = "AreaMap:\n" ++ intercalate "\n" am

-- Remove X's and direction chars.
cleanup_x = map $ \c -> if (c == 'X' || c `elem` dir_chars) then '.' else c

-- p1 minus p2.
subtract :: Point -> Point -> Point
subtract p1 p2 = let Point x1 y1 = p1
                     Point x2 y2 = p2
                 in
                 Point (x1 - x2) (y1 - y2)

-- Get a point that steps in the direction of p2 from p1.
get_delta_point p1 p2 =
    let Point x1 y1 = p1
        Point x2 y2 = p2
        sign_x = sign(x2 - x1)
        sign_y = sign(y2 - y1)
    in
    Point (x1 + sign_x) (y1 + sign_y)

-- Find the given char in a list of strings and return the coordinates.
find_char :: Char -> [String] -> Maybe Point
find_char c strs
    | null strs = Nothing
    | found /= -1 = Just $ Point found 0
    | otherwise = find_char c (tail strs) >>= next
    where
        found = idxof c $ head strs
        next = (\p ->
                let Point next_x next_y = p in
                if found /= -1
                then Just $ Point found 0
                else Just $ Point next_x (1 + next_y))

-- Find and read the guard's state from the map. Assumes she's in start state ('^').
get_guard :: AreaMap -> Maybe Guard
get_guard am
    | null m = trace "No map given." Nothing
    | found /= -1 = Just $ Guard (Point found 0) FaceUp False
    | otherwise = Just $ Guard (Point next_x (1 + next_y)) next_dir looping
    where
        AreaMap m = am
        found = idxof '^' (head m)
        Just (Guard (Point next_x next_y) next_dir looping) = get_guard $ AreaMap (tail m)

-- Is the point in bounds of the map?
p_in_bounds :: Point -> AreaMap -> Bool
p_in_bounds p am = xinb && yinb
    where
        xinb = (0 <= x && x < (length $ head m))
        yinb = (0 <= y && y < length m)
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
    | not (p_in_bounds p am)            = Nothing
    | char_at_point p am == Just '#'    = Just p
    | d == FaceLeft                     = next_collision (Point (x - 1) y) d am
    | d == FaceDown                     = next_collision (Point x (y + 1)) d am
    | d == FaceUp                       = next_collision (Point x (y - 1)) d am
    | d == FaceRight                    = next_collision (Point (x + 1) y) d am
    where
        AreaMap m = am
        Point x y = p

-- Return a new map with the given point painted over with an X.
draw_point :: Char -> Point -> AreaMap -> Maybe AreaMap
draw_point c p am
    | null m || not (p_in_bounds p am) = Nothing
    | otherwise = Just $ AreaMap (top ++ ([midleft ++ c:midright]) ++ bot)
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
    | p1 == p2 = if (curr_is_dir_char == Just True) then Just am else draw_point 'X' p1 am
    | curr_is_dir_char == Just True = next_draw am
    | otherwise = draw_point 'X' p1 am >>= next_draw
    where
        is_dir_char = \c -> Just $ c `elem` dir_chars
        curr_is_dir_char = (char_at_point p1 am >>= is_dir_char)
        AreaMap m = am
        Point x1 y1 = p1
        Point x2 y2 = p2
        dx = sign (x2 - x1)
        dy = sign (y2 - y1)
        next_draw = draw_line (Point (x1 + dx) (y1 + dy)) p2

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

-- Get the path slice between two points on a map.
get_path_slice :: Point -> Point -> AreaMap -> Maybe String
get_path_slice p1 p2 am
    | null m    = Nothing
    | p1 == p2  = Just $ catMaybes ((char_at_point p1 am):[])
    | otherwise = do
        here <- char_at_point p1 am
        rest <- get_path_slice next_p p2 am
        return (here:rest)
    where
        AreaMap m = am
        next_p = get_delta_point p1 p2

-- Map a 'default map' and a drawn-on map (a map with a guard's path) to a list of new maps, where each
-- square the guard has touched is mapped to a new obstacle.
map_to_loop_attempts :: AreaMap -> AreaMap -> Maybe [AreaMap]
map_to_loop_attempts dm am
    | null m || isNothing found = Nothing
    | found /= Nothing          = do
        found    :: Point <- found
        next_am  :: AreaMap <- draw_point '.' found am
        this_am  :: AreaMap <- draw_point '#' found dm
        start    :: Point <- start_point
        this_am' :: AreaMap <- draw_point '.' start this_am
        let rest :: Maybe [AreaMap] = map_to_loop_attempts dm next_am
        return (this_am':(if isNothing rest then [] else (let (Just rest') = rest in rest')))
    where
        AreaMap m   = am
        cleaned am  = let AreaMap m = am in AreaMap $ map (\s -> cleanup_x s) m
        found       = find_char 'X' m
        start_point = find_char '^' m

-- Same as above but put down markers for when the guard changes directions. Used for loop detection.
walk_map :: Guard -> AreaMap -> Maybe (Guard, AreaMap)
walk_map g am
    | char_at_point curr_pos am == (Just '#') = trace "walk_map: guard inside obstacle" $ Nothing
    | not (p_in_bounds curr_pos am) = trace "walk_map: guard oob" $ Just (g, am)
    | (Just $ char_from_dir curr_dir) == (char_at_point curr_pos am) = Just (Guard curr_pos curr_dir True, am)
    | otherwise = do
        next_pos <- if next_coll == Nothing then edge else next_coll
        next_map <- draw_line curr_pos next_pos am
        next_map' <- draw_point (char_from_dir curr_dir) curr_pos next_map
        return (Guard next_pos (rotate_right curr_dir) looping, next_map')
    where
        Guard curr_pos curr_dir looping = g
        coll = next_collision curr_pos curr_dir am
        next_coll = coll >>= (\c -> Just $ approach_obstacle c curr_dir)
        next_dir = rotate_right curr_dir
        edge = Just $ find_edge curr_pos curr_dir am

-- Walk until the guard is off the mapped area.
keep_walking :: Guard -> AreaMap -> Maybe (Guard, AreaMap)
keep_walking g am
    | not $ p_in_bounds g_pos am || looping = trace "guard looping or oob" $ Just (g, am)
    | char_here `elem` dir_chars = Just (Guard g_pos g_dir True, am) -- It loops
    | otherwise = walk_map g am >>= (\t -> keep_walking (fst t) (snd t))
    where
        Guard g_pos g_dir looping = g
        AreaMap m = am
        Just char_here = char_at_point g_pos am
        char_currdir = Just (char_from_dir g_dir)

-- -- Takes an AreaMap with turns on it and
-- filter_loops :: AreaMap -> AreaMap

-- Did the guard loop?
did_loop :: Guard -> Bool
did_loop g = let Guard _ _ looped = g in looped

-- I flipped something somewhere idk
char_from_dir :: Dir -> Char
char_from_dir d = case d of
    FaceUp      -> '^'
    FaceDown    -> 'v'
    FaceLeft    -> '<'
    FaceRight   -> '>'

task1 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle

    let lines = split '\n' contents
    let am = AreaMap lines
    let Just start = find_char '^' lines
    let Just am' = draw_point '.' start am
    let Just g = get_guard am
    let Just (Guard p d looped, AreaMap walked) = keep_walking g am'

    let replace = map $ \c -> if c `elem` dir_chars then 'X' else c
    let walked' = map (\s -> replace s) walked

    print ("Task 1 result: " ++ show (sum $ map (count 'X') walked'))

task2 input = do
    handle <- openFile input ReadMode
    contents <- hGetContents handle

    let lines = split '\n' contents
    let am = AreaMap lines
    let Just start = find_char '^' lines
    let Just am' = draw_point '.' start am
    let Just g = get_guard am
    let Just (Guard p d looped, AreaMap walked) = keep_walking g am'

    let replace = map $ \c -> if c `elem` dir_chars then 'X' else c
    let AreaMap walked' = AreaMap $ map (\s -> replace s) walked
    let Just (AreaMap walked'') = draw_point '^' start (AreaMap walked')

    -- walked'' has all X's and the start marker. Now replace this with a list of AreaMaps where
    -- each has one X replaced with # and the rest replaced.
    -- There are 4756 of these in the input.
    let Just attempts = map_to_loop_attempts am (AreaMap walked'')

    -- Now map to keep_walking.
    let keep_walked = catMaybes $ map (keep_walking g) attempts
    let guard_states = map fst keep_walked
    let enumerated_gs = zip [0..] (map fst keep_walked)
    prall enumerated_gs
    let loop_count = length $ filter did_loop guard_states

    print ("Task 2 result: " ++ show loop_count)

main = do
    -- task1 "input/day6-input.txt"
    task2 "input/day6-input.txt"
