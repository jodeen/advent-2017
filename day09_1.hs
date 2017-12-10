
type InGarbage = Bool
type InEscape = Bool
type State = (InGarbage, InEscape, Int, Int)

handleChar :: State -> Char -> State
handleChar (True, False, depth, count) '!' = (True, True, depth, count)
handleChar (True, True, depth, count) _ = (True, False, depth, count)
handleChar (_, False, depth, count) '<' = (True, False, depth, count)
handleChar (True, False, depth, count) '>' = (False, False, depth, count)
handleChar (True, False, depth, count) _ = (True, False, depth, count)
handleChar (False, False, depth, count) '{' = (False, False, depth + 1, count)
handleChar (False, False, depth, count) '}' = (False, False, if depth > 0 then depth - 1 else depth, if depth > 0 then count + depth else count)
handleChar state _ = state

parse :: String -> State
parse text = foldl handleChar (False, False, 0, 0) text

main = do
    content <- readFile "day09.txt"
    let (_, _, _, count) = parse content
    return count