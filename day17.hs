import Data.List

doStep :: Int -> Int-> [Int] -> [Int]
doStep stepAmount currentStep xs = cycle current
    where 
        cycled = drop stepAmount xs
        current = take currentStep cycled ++ [currentStep]

doLoop :: Int -> Int -> [Int] -> [Int]
doLoop _ 2018 xs = take 2017 xs
doLoop stepAmount currentStep xs = doLoop stepAmount (currentStep + 1) (doStep stepAmount currentStep xs)

doLoop2 :: Int -> Int -> [Int] -> [Int]
doLoop2 _ 50000000 xs = take 50000000 xs
doLoop2 stepAmount currentStep xs = doLoop2 stepAmount (currentStep + 1) (doStep stepAmount currentStep xs)


doOne :: Int -> Int
doOne stepAmount = head (doLoop stepAmount 1 [0])