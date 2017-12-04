num = 12

oddSquares = [x*x | x<-[1,3..]];
nextLargestSquare :: Int -> Int
nextLargestSquare x = head(dropWhile (<x) oddSquares)

box :: Int -> (Int, (Int, Int, Int, Int))
box 1 = (0, (1,1,1,1))
box x = (boxNum, (corner - 3 * boxSize,corner - 2 * boxSize, corner - boxSize, corner ))
    where 
        corner = nextLargestSquare x
        boxSize = (ceiling (sqrt (fromIntegral corner))) - 1
        boxNum = if odd boxSize then quot (boxSize+1) 2 else quot boxSize 2

side :: Int -> (Int, Int, Int, Int) -> (Int, Int)
side x (a,b,c,d) | x <= a = (a - (b-a) + 1, a)
                 | b >= x = (a,b)
                 | c >= x = (b,c)
                 | otherwise = (c,d)

middleSide :: (Int, Int) -> Int
middleSide (a,b) = a + (quot (b-a) 2)

doAll :: Int -> Int
doAll x = boxNum + abs (middle - x)
        where
            (boxNum, borders) = box x
            middle = middleSide (side x borders)


