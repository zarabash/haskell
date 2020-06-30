module Main (get_maze, print_maze, is_wall, place_player, move, can_move, game_loop, get_path, main) where 

import System.Environment

maze_path = "overwrite this with your own path!"

-- Useful code from Lecture 25
-- You may use this freely in your solutions

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x 

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char = 
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze

---- Part A

-- Question 1

get_maze :: String -> IO [String]
-- Store file content into str
get_maze file = do
            str <- readFile file
            return (lines str)

-- Question 2

print_maze :: [String] -> IO ()
-- Print maze in new line
print_maze maze = do
            let p = unlines maze
            putStrLn p

-- Question 3

is_wall :: [String] -> (Int, Int) -> Bool
-- If there is # then it is a wall
is_wall m (x,y) 
           |get m x y == '#' = True
           |otherwise = False

-- Question 4

place_player :: [String] -> (Int, Int) -> [String]
-- Returns @ as player at set coordinate
place_player m (x,y) = set m x y '@'


---- Part B

-- Question 5

move :: (Int, Int) -> Char -> (Int, Int)
-- Move player if either one wasd letters were entered, other than that ignore
move (x,y) c 
           |c == 'w' = (x,y-1)
           |c == 's' = (x,y+1)
           |c == 'a' = (x-1,y)
           |c == 'd' = (x+1,y)
           |otherwise = (x,y)
-- Question 6

can_move :: [String] -> (Int, Int) -> Char -> Bool
-- move function returns a coordinate then is_wall checks if it is #
can_move m (x,y) c
           |is_wall m (userCoordinate) = False
           |otherwise = True
           where userCoordinate = move (x,y) c

-- Question 7

game_loop :: [String] -> (Int, Int) -> IO ()
game_loop m (x,y) = do
           print_maze $ place_player m (x,y) -- Prints maze with player placed
           putStrLn "Move by entering W, A, S or D only"
           userInput <- getLine
           let (x1,x2) = if can_move m (x,y) (head userInput) == True then move (x,y) (head userInput) else (x,y) 
           game_loop m (x1,x2)
          

---- Part C

-- Question 8

get_moves maze pos visited = 
    let 
        nPos = move pos 'w'
        ePos = move pos 'd'
        sPos = move pos 's'
        wPos = move pos 'a'
    in
        foldr (\newPos r -> 
               if is_wall maze newPos || newPos `elem` visited
               then r else newPos : r )
              [] [nPos,ePos,sPos,wPos]

--              maze        visited         pos          goal  
depth_first :: [String] -> [(Int,Int)] -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
depth_first maze visited goal pos = 
    let 
        found = pos == goal
        posMoves = (get_moves maze pos visited) 
        allSearches = map (depth_first maze (pos : visited) goal) posMoves
        succSearch = filter (\x -> goal `elem` x) allSearches
        next = if length succSearch == 0 then [] else succSearch!!0
    in
        if found then [pos]
        else pos : next

get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path maze start end = depth_first maze [] end start

-- Question 9

place_path maze path = foldr (\(x,y) m -> set m x y '.')
                              maze path


get_goal :: [String] -> (Int,Int)
get_goal maze =
    let
        y = length maze
        x = length (maze!!0)
    in
        (x-2,y-2)

main :: IO ()
main = do
    args <- getArgs
    let filename     = args!!0
    maze <- get_maze filename
    let goal         = get_goal maze
    let path         = get_path maze (1,1) goal
    let mazeWithPath = place_path maze path
    print_maze mazeWithPath


