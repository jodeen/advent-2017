
import Data.List
import Data.Bits

lowMask = 65535

nextItem :: Int -> Int -> Int
nextItem factor prev = mod (prev * factor) 2147483647

lowMatch :: (Int, Int) -> Bool
lowMatch (a,b) = (a .&. lowMask) == (b.&.lowMask)

isDif :: Int -> Int -> Bool
isDif by test = (mod test by) == 0


doOne :: (Int, Int) -> Int -> Int
doOne (aSeed, bSeed) iters = length (filter lowMatch (take iters (zip genA genB)))
    where 
        genA = tail (iterate (nextItem 16807) aSeed)
        genB = tail (iterate (nextItem 48271) bSeed)

doTwo :: (Int, Int) -> Int -> Int
doTwo (aSeed, bSeed) iters = length (filter lowMatch (take iters (zip genA genB)))
    where 
        genA = filter (isDif 4) (tail (iterate (nextItem 16807) aSeed))
        genB = filter (isDif 8) (tail (iterate (nextItem 48271) bSeed))