import Data.List.Split
import Data.List

testData = [0, 2, 7, 0] :: [Int]

maxIdx :: [Int] -> Int
maxIdx mem = case elemIndex (maximum mem) mem of Just a -> a

incHead :: [Int] -> [Int]
incHead xs = ((head xs) + 1) : (tail xs)

distributeVal :: Int -> Int -> [Int] -> [Int]
distributeVal _ 0 values = values
distributeVal startAt value values 
    | startAt == (length values) = distributeVal 0 value values
    | otherwise = distributeVal (startAt + 1) (value - 1) (a ++ (incHead b))
    where 
        (a,b) = splitAt startAt values

distribute :: ([Int], [[Int]])-> ([Int],[[Int]])
distribute (values, previous) = (distributeVal (idx+1) (values !! idx) newValues, values:previous)
    where
        idx = maxIdx values
        (a,b) = splitAt idx values
        newValues = a ++ (0 : tail b)


doOne :: [Int] -> Int
doOne values = length (snd (head (dropWhile (\(last, all) -> not (elem last all)) (iterate distribute (values, [])))))

doTwo :: [Int] -> Int
doTwo values = a + 1
        where
            (last, all) = head (dropWhile (\(last, all) -> not (elem last all)) (iterate distribute (values, [])))
            Just a = elemIndex last all


main = do
    content <- readFile "day06.txt"
    let mem = map (\x -> read x :: Int) (splitOn "\t" content)
    return (doOne mem, doTwo mem)