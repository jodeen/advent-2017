import Data.List.Split
import Data.List



distribute :: [Int] -> [Int]
distribute mem = [maxVal, maxElem]
    where 
        maxVal = maximum mem
        maxElem = elemIndex maxVal mem
        doDist :: Int -> [Int] -> [Int]
        doDist 0 xs = xs
        doDist amt [] = doDist amt mem
        doDist amt xs = 

main = do
    content <- readFile "day06.txt"
    let mem = map (\x -> read x :: Int) (splitOn "\t" content)
    return mem