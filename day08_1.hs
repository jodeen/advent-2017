import Data.List.Split
import Data.List
import qualified Data.Map.Strict as Map

type Condition = (String, String, Int)
type RegisterOp = (String, String, Int)
type Instruction = (RegisterOp, Condition) 
type Registers = Map.Map String Int

l = "tup inc 48 if rz == -375"
t = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"

parseLine::String -> Instruction
parseLine s = (parseRegister start, parseCondition end)
    where 
        [start, end] = splitOn " if " s

parseCondition :: String -> Condition
parseCondition s = (reg, cmp, read amt) 
            where
                [reg, cmp, amt] = splitOn " " s

parseRegister :: String -> RegisterOp
parseRegister s = (reg, oper, read amt)
    where
        [reg, oper, amt] = splitOn " " s

parse :: [String] -> [Instruction]
parse s = map parseLine s

processCondition :: Condition -> Registers -> Bool
processCondition (regName, "==", amt) regs = (Map.findWithDefault 0 regName regs) == amt
processCondition (regName, ">=", amt) regs = (Map.findWithDefault 0 regName regs) >= amt
processCondition (regName, "<=", amt) regs = (Map.findWithDefault 0 regName regs) <= amt
processCondition (regName, ">", amt) regs = (Map.findWithDefault 0 regName regs) > amt
processCondition (regName, "<", amt) regs = (Map.findWithDefault 0 regName regs) < amt
processCondition (regName, "!=", amt) regs = (Map.findWithDefault 0 regName regs) /= amt

processRegister :: RegisterOp -> Registers -> Registers
processRegister (regName, "inc", amt) regs = incrementReg regName amt regs
processRegister (regName, "dec", amt) regs = incrementReg regName (-1 * amt) regs

incrementReg :: String -> Int -> Registers -> Registers
incrementReg key val regs = case Map.lookup key regs of
    Nothing -> Map.insert key val regs
    Just old -> Map.insert key (val + old) regs

processLine :: Registers -> Instruction -> Registers
processLine regs (register, condition)  = if processCondition condition regs then processRegister register regs else regs

doProcess :: [Instruction] -> Registers
doProcess list = foldl processLine Map.empty list

main = do
    content <- readFile "day08.txt"
    let parsed = parse (lines content)
    let regs = doProcess parsed
    let m  =Map.foldl max 0 regs
    return m