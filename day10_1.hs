
import Data.List.Split

size = 256
initList = [0..size-1]

type State = (Int, [Int])

step :: Int -> [Int]
step x  = undefined


doReverse :: Int-> [Int] -> [Int]
doReverse len list = rem ++ (reverse begin)
    where 
        (begin, rem) = splitAt len (take size list)

doSkip :: Int -> [Int] -> [Int]
doSkip amount xs = rem ++ skip
        where
            (skip, rem) = splitAt amount xs

doStep :: State -> Int -> State
doStep (skip, xs) len = (skip + 1, doSkip skip (doReverse len xs))

process :: [Int] -> [Int]
process xs = take fullSize (drop (fullSize - (mod (s + sumSkip) fullSize)) (cycle res))
    where 
        (lastSkip, res) = foldl doStep (0, initList) xs
        fullSize = length initList
        s = sum xs
        sumSkip = quot (lastSkip * (lastSkip - 1)) 2


res = process [165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153]
soln = (res !! 0) * (res !! 1)

