import Data.List
import Data.List.Split

spin :: Int -> [Char] -> [Char]
spin idx xs = b ++ a
    where
        (a,b) = splitAt ((length xs) - idx) xs

type Instruction = ([Char] -> [Char])

setIdx :: Int -> Char -> [Char] -> [Char]
setIdx idx val xs = front ++ [val] ++ end
    where
        (front, (oldVal:end)) = splitAt idx xs


swap :: Int -> Int -> [Char] -> [Char]
swap a b xs = setIdx a oldB (setIdx b oldA xs)
    where 
        oldA = xs !! a
        oldB = xs !! b

swapVal :: Char -> Char -> [Char] -> [Char]
swapVal a b xs = swap aIdx bIdx xs
        where 
            Just aIdx = elemIndex a xs
            Just bIdx = elemIndex b xs

parseEntry :: String -> Instruction
parseEntry ('p':xs) = swapVal (one !! 0) (two !! 0)
    where (one:two:[]) = splitOn "/" xs
parseEntry ('s':xs) = spin (read xs :: Int)
parseEntry ('x':xs) = swap (read one::Int) (read two::Int) 
    where (one:two:[]) = splitOn "/" xs

doOne :: [Instruction] -> String
doOne entries = foldl (\acc f -> f acc) ['a'..'p'] entries

doSeed :: [Instruction] -> String -> String
doSeed entries seed = foldl (\acc f -> f acc) seed entries

type Result = ([String], String, Int, Bool)

doStep :: [Instruction] -> Result -> Result
doStep entries (results, seed, count, _) = if result `elem` results 
    then (results, result, count + 1, True) 
    else (newResults, result, count + 1, False)
    where
        result = foldl (\acc f -> f acc) seed entries
        newResults =  results ++ [result]
 

doTwo :: [Instruction] -> Result
doTwo entries = head (dropWhile (\(_,_,_,x) -> not x) (iterate (doStep entries) ([['a'..'p']], ['a'..'p'], 0, False)))


main = do
    contents <- readFile "day16.txt"
    let entries = map parseEntry (splitOn "," contents)
    print (doSeed entries ['a'..'p'])
    print (doSeed entries (doSeed entries ['a'..'p']))
    print (doSeed entries (doSeed entries (doSeed entries ['a'..'p'])))
    print (doSeed entries "picgbahlnkfmojde")
    let (results,_,_, _) = doTwo entries
    print (results)
    print (results !! (mod 1000000000 (length results)  ))