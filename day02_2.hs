import Data.List.Split
import System.Environment

parseLine :: String -> [Int]
parseLine x = map (\x -> read x::Int) (splitOn "\t" x)

parse :: String -> [[Int]]
parse x = map parseLine (splitOn "\n" x)

findDiv :: [Int] -> (Int, Int)
findDiv x = head (filter (\f -> mod (fst f) (snd f) == 0) [(a,b) | a <- x, b <- x, a>b])

ranges :: [[Int]] -> [(Int,Int)]
ranges x = map findDiv x

calc :: [(Int,Int)] -> Int
calc l = sum (map (\x -> quot (fst x)  (snd x)) l )

main = do 
    s <- readFile "day02.txt"
    let parsed = parse s
    let r = ranges parsed
    let c = calc r
    return c
    