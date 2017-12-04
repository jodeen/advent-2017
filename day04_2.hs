
import Data.List.Split
import Data.List

splitWords :: String -> [String]
splitWords s = words s

isValidPhrase :: [String] -> Bool
isValidPhrase l = length s == length (nub s)
    where 
        s = map sort l

main = do
    content <- readFile "day04.txt"
    let fileLines = lines content
    let good = length (filter isValidPhrase (map splitWords fileLines))
    return good