
import Data.List.Split
import Data.List

type State = (Int, Int, [Int])

doUpdate :: State -> State
doUpdate (count, position, []) = (count, position, [])
doUpdate (count, position, mem) = doUpdate (count + 1, newPosition, newMem)
    where
        action = mem !! position
        newPosition = position + action
        (a,b) = splitAt (position) mem
        newMem = if newPosition >=0 && newPosition < (length mem) then a ++ [action + 1] ++ tail b else []


main = do
    contents <- readFile "day05.txt" 
    let mem = map read (lines contents)
    return (doUpdate (0, 0, mem))