
import Data.List.Split

import Data.Sequence

type State = (Int, Int, Seq Int)

doUpdate :: State -> State
doUpdate (count, position, mem) 
    | (Data.Sequence.length mem) > 0 = doUpdate $! (count + 1, newPosition, newMem) 
    | otherwise = (count, position, mem)
    where
        action = index mem position
        newPosition = position + action
        newMem = if newPosition >=0 && newPosition < (Data.Sequence.length mem) then update position (newCell action) mem else empty

newCell :: Int -> Int
newCell x = if x >= 3 then x-1 else x+1
main = do
    contents <- readFile "day05.txt" 
    let mem = fromList $! (map read (lines contents))
    let (a,b,c)= (doUpdate (0, 0, mem))
    return a