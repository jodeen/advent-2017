import Data.List.Split
import System.Environment

parseLine :: String -> [Int]
parseLine x = map (\x -> read x::Int) (splitOn "\t" x)

parse :: String -> [[Int]]
parse x = map parseLine (splitOn "\n" x)

ranges :: [[Int]] -> [(Int,Int)]
ranges x = map (\i -> (minimum i, maximum i)) x

calc :: [(Int,Int)] -> Int
calc l = sum (map (\x -> snd x - fst x) l )

main = do 
    s <- readFile "day02.txt"
    let parsed = parse s
    let r = ranges parsed
    let c = calc r
    return c
    