import Data.List

testData = ["     |          ",
            "     |  +--+    ",
            "     A  |  C    ",    
            " F---|----E|--+ ",
            "     |  |  |  D ", 
            "     +B-+  +--+ "]

data Dir = Left' | Right' | Up | Down deriving (Show)
type State = (Dir, (Int, Int), [Char], Bool)

initialState :: String -> State
initialState row = case elemIndex '|' row of
    Just x -> (Down, (x, 0), [], False)
    Nothing -> undefined

downChar :: (Int, Int) -> [String] -> Char
downChar (x,y) rows = (rows !! (y+1)) !! x

leftChar :: (Int, Int) -> [String] -> Char
leftChar (x,y) rows = (rows !! (y)) !! (x-1)

rightChar :: (Int, Int) -> [String] -> Char
rightChar (x,y) rows = (rows !! (y)) !! (x+1)

upChar :: (Int, Int) -> [String] -> Char
upChar (x,y) rows = (rows !! (y-1)) !! (x)


follow :: [String] -> State -> State
follow rows (Down, (x,y), items, _) 
    | (y < (length rows) - 1) && (down  /= ' ') = (Down, (x, y+1), down : items, False)
    | x >0 && (left  /= ' ') = (Left', (x-1, y), left : items, False)
    | (x < (length row) - 1) && (right /= ' ') = (Right', (x+1,y), right : items, False)
    | otherwise = (Down, (x,y), items, True)
    where
        row = rows !! y
        down = downChar (x,y) rows
        left = leftChar (x,y) rows
        right = rightChar (x,y) rows


follow rows (Up, (x,y), items, _) 
    | (y > 0) && (up  /= ' ') = (Up, (x, y-1), up : items, False)
    | x >0 && (left  /= ' ') = (Left', (x-1, y), left : items, False)
    | (x < (length row) - 1) && (right /= ' ') = (Right', (x+1,y), right : items, False)
    | otherwise = (Up, (x,y), items, True)
    where
        row = rows !! y
        down = downChar (x,y) rows
        left = leftChar (x,y) rows
        right = rightChar (x,y) rows
        up = upChar (x,y) rows

follow rows (Left', (x,y), items, _) 
    | (x > 0) && (left  /= ' ') = (Left', (x-1, y), left : items, False)
    | y >0 && (up  /= ' ') = (Up, (x, y-1), up : items, False)
    | (y < (length rows) - 1) && (down /= ' ') = (Down, (x,y+1), down : items, False)
    | otherwise = (Left', (x,y), items, True)
    where
        row = rows !! y
        down = downChar (x,y) rows
        left = leftChar (x,y) rows
        right = rightChar (x,y) rows
        up = upChar (x,y) rows 
    
follow rows (Right', (x,y), items, _) 
    | (x < (length row) -1) && (right  /= ' ') = (Right', (x+1, y), right : items, False)
    | y >0 && (up  /= ' ') = (Up, (x, y-1), up : items, False)
    | (y < (length rows) - 1) && (down /= ' ') = (Down, (x,y+1), down : items, False)
    | otherwise = (Right', (x,y), items, True)
    where
        row = rows !! y
        down = downChar (x,y) rows
        left = leftChar (x,y) rows
        right = rightChar (x,y) rows
        up = upChar (x,y) rows      

cleanItems :: String -> String
cleanItems xs = reverse (filter (\x -> elem x ['A'..'Z']) xs)

doOne :: [String] -> String
doOne rows = cleanItems items
    where (_, _, items, _) = head (dropWhile (\(_,_,_,found) -> not found) (iterate (follow rows) (initialState (rows !! 0))))

doTwo :: [String] -> Int
doTwo rows = (length items) + 1
        where (_, _, items, _) = head (dropWhile (\(_,_,_,found) -> not found) (iterate (follow rows) (initialState (rows !! 0))))

main = do
    contents <- readFile "day19.txt"
    return ((doOne (lines contents)), doTwo (lines contents))