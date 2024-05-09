{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Main where
import Data.List (sortBy)
import Data.Ord (comparing)

data Register = R0 | R1 | R2 | R3 | R4
    deriving (Show, Enum, Eq, Ord)

type Address = Int
newtype Immediate = Imm Int deriving (Show, Eq)

data Operand where
    Reg :: Register -> Operand
    Val :: Immediate -> Operand
    Addr :: Address -> Operand
deriving instance Show Operand

data Instruction where
    -- Register to Register Operations
    MOVR :: Register -> Register -> Instruction
    MOVV :: Register -> Immediate -> Instruction

    -- Arithmetic Operations
    ADD :: Register -> Register -> Register -> Instruction
    SUB :: Register -> Register -> Register -> Instruction

    -- Stack Operations
    PUSH :: Operand -> Instruction
    POP :: Register -> Instruction

    -- Jump and Call Operations
    JP :: Address -> Instruction
    JL :: Register -> Register -> Address -> Instruction
    CALL :: Address -> Instruction

    -- IO and Control Instructions
    RET :: Instruction
    PRINT :: Operand -> Instruction
    HALT :: Instruction
    
deriving instance Show Instruction

data VMState = VMState {
    pc      :: Int,
    regs    :: [(Register, Int)],
    stack   :: [Operand],
    program :: [Instruction],
    halted  :: Bool,
    output  :: Maybe Int
} deriving (Show)

-- Helper functions for register management
lookupReg :: Register -> [(Register, Int)] -> Int
lookupReg reg regList = maybe 0 id $ lookup reg regList

updateReg :: Register -> Int -> [(Register, Int)] -> [(Register, Int)]
updateReg reg newVal regList = (reg, newVal) : filter ((/= reg) . fst) regList

extractOperandValue :: Operand -> [(Register, Int)] -> Int
extractOperandValue (Reg r) regs = lookupReg r regs
extractOperandValue (Val (Imm i)) _ = i
extractOperandValue (Addr a) _ = a

execute :: Instruction -> VMState -> VMState
execute HALT state = state { halted = True }

execute (MOVR dst src) state =
    let srcVal = lookupReg src (regs state)
    in state { regs = updateReg dst srcVal (regs state) }
    
execute (MOVV dst (Imm value)) state =
    state { regs = updateReg dst value (regs state) }
    
execute (ADD dst src1 src2) state =
    let val1 = lookupReg src1 (regs state)
        val2 = lookupReg src2 (regs state)
        newVal = val1 + val2
    in state { regs = updateReg dst newVal (regs state) }
    
execute (SUB dst src1 src2) state =
    let val1 = lookupReg src1 (regs state)
        val2 = lookupReg src2 (regs state)
        newVal = val1 - val2
    in state { regs = updateReg dst newVal (regs state) }
    
execute (PUSH op) state =
    state { stack = op : stack state }
    
execute (POP dst) state =
    case stack state of
        [] -> state
        (op:ops) -> state { regs = updateReg dst (extractOperandValue op (regs state)) (regs state), stack = ops }
        
execute (JP addr) state = 
    state { pc = addr }
    
execute (JL r1 r2 addr) state =
    let r1Val = lookupReg r1 (regs state)
        r2Val = lookupReg r2 (regs state)
    in if r1Val < r2Val then state { pc = addr } else state
    
execute (CALL addr) state =
    let returnAddress = pc state
    in state { stack = Addr returnAddress : stack state, pc = addr }
    
execute (RET) state =
    case stack state of
        (Addr retAddr:ops) -> state { pc = retAddr, stack = ops }
        _ -> state { halted = True }
        
execute (PRINT op) state =
    state { output = Just (extractOperandValue op (regs state)) }

runVM :: VMState -> IO VMState
runVM state@VMState { halted = True } = return state
runVM state@VMState { pc = pcVal, program = prog } = do
    let instruction = prog !! pcVal
        newState = execute instruction state { pc = pcVal + 1, output = Nothing }
    case output newState of
        -- There are comments here that can be disabled to help debugging the VM. I have turned them off so the output is not that busy.
        Just val -> print val {- >>  print (sortRegisters $ regs state) -} >> runVM (newState { output = Nothing })
        Nothing  -> {-  print (sortRegisters $ regs state) >> print (state) >> -} runVM newState

initialize :: [Int] -> VMState
initialize code = VMState {
    pc      = 0,
    regs    = [(r, 0) | r <- [R0 .. R4]],
    stack   = [],
    program = code,
    halted  = False,
    output  = Nothing
}

sortRegisters :: [(Register, Int)] -> [(Register, Int)]
sortRegisters = sortBy (comparing fst)

programSumN :: [Int]
programSumN =
    [ 11, 0, 0          -- MOVV R0, 0 (Set R0 = 0 to sum)
    , 11, 1, 1          -- MOVV R1, 1 (Starts the acc = 1)
    , 11, 2, 5          -- MOVV R2, 5 (Set R2 = 5 (N))
    , 20, 0, 0, 1       -- ADD R0, R0, R1
    , 11, 3, 1          -- MOVV R3, 1 (acc++)
    , 20, 1, 1, 3       -- ADD R1, R1, R3 (R1 += 1)
    , 41, 1, 2, 3       -- JL R1, R2, 3
    , 60, 0             -- PRINT R0
    , 99                -- HALT
    ]

programAddTwoNums :: [Int]
programAddTwoNums =
    [ 11, 1, 3          -- MOVV R1, 3 
    , 11, 2, 4          -- MOVV R2, 4
   
    , 42, 5             -- CALL 5
    , 60, 0
    , 99                -- HALT

    , 30, 1             -- PUSH R1 at 4
    , 30, 2             -- PUSH R2
    , 31, 3             -- POP R3
    , 31, 4             -- POP R4
    , 20, 0, 3, 4       -- ADD  R0, R3, R4
    , 50                -- RET
    ]

programFib :: [Int]
programFib = 
    [ 11, 0, 10          -- MOVV R0, 10 (Calculate Fibonacci for n=10)
    , 11, 1, 0           -- MOVV R1, 0 (Fibonacci(0))
    , 11, 2, 1           -- MOVV R2, 1 (Fibonacci(1))
    , 11, 3, 2           -- MOVV R3, 2 (To check if the calculation should continue)
    , 41, 0, 3, 12       -- JL R0, R3, Addr 12  (if R0 < R3 jump to Addr 12 - print R1 as result)
    , 20, 1, 1, 2        -- ADD R1, R1, R2 (R1 = R1 + R2; New Fibonacci number)
    , 10, 3, 2           -- MOVR R3, R2  (R3 = previous R2)
    , 10, 2, 1           -- MOVR R2, R1  (R2 = previous R1)
    , 10, 1, 3           -- MOVR R1, R3  (R1 = previous R2 stored in R3)
    , 11, 3, 1           -- MOVV R3, 1   (To decrement R0)
    , 21, 0, 0, 3        -- SUB R0, R0, R3 (R0 -= 1)
    , 40, 4              -- JP Addr 4    (jump back to the start of loop)
    , 60, 1              -- PRINT R1
    , 99                 -- HALT
    ]

programError :: [Int]
programError = [77, 11, 0, 10]

main :: IO ()
main = do
    let program = programFib
    let initialState = initialize program
    finalState <- runVM initialState
    print (sortRegisters $ regs finalState)
