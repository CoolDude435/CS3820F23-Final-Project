{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Justinwork where
import Data.Int
import Data.Word
import Parser
import Syntax
import Data.Bits
import GHC.Float

import Data.Fixed (mod') --Floating point rem

import Debug.Trace

data WasmState = WasmState {stack :: [Word32],
                            locals :: [Word32],
                            globals :: [Word32],
                            functions :: [Function], --this doesn't change
                            instrs :: [Instr]
                            --types :: [FuncType] --this doesn't change
                            }
                            deriving (Eq, Show)


jlinInterp :: Module -> [Word32] -> [Word32]
--jlinInterp m p = trace (show m) p
jlinInterp (Module fs ts gs st) p = steps (step (WasmState (reverse p) p (initGlobals gs) fs [(Control (Call st))]))

--jlinInterp (Module fs ts gs st) p = steps (step (WasmState (p `stackPop` (numParams (fs `getAtIndex` st))) p (initGlobals gs) fs [(Control (Call st))]))
--don't think we need to prep the stack since our interpretation of Call will setup our stack for our function call automatically

--Initializes globals to be used in a WasmState
initGlobals :: [([Instr], ValType)] -> [Word32]
initGlobals gs = map initGlobal gs

initGlobal :: ([Instr], ValType) -> Word32
initGlobal (is, I32) = case (steps (WasmState [] [] [] [] is)) of
                          ((ts:s)) -> ts

localPlaceholders :: Int -> [Word32]
localPlaceholders 1 = []
localPlaceholders n = 0:(localPlaceholders (n-1))

{-Runs a function when there is a call instruction
Create a new WasmState for the function which has a new stack, locals, and instructions from the function
Locals is first populated with the # of parameters passed in (from FuncType)
branchSteps the new function WasmState, then we pop the # of results needed (from FuncType) off its stack onto
the original stack and keep the new list of globals *(Globals may have been changed in the function)
finally return the resulting WasmState-}
evalFunc :: WasmState -> FuncIdx -> WasmState
evalFunc (WasmState s ls gs fs is) fIdx = case (branchSteps (WasmState [] (s `stackPop` (numParams (fs `getAtIndex` fIdx))++(localPlaceholders (funcNumLTypes (fs `getAtIndex` fIdx)))) gs fs (funcInstr (fs `getAtIndex` fIdx)))) of
                                              (WasmState s' ls' gs' fs' is') -> (WasmState ((take (numReturns (fs `getAtIndex` fIdx)) s')++(drop (numParams (fs `getAtIndex` fIdx)) s)) ls gs' fs is)
                                              --(WasmState s' ls' gs' fs' is') -> (WasmState ((s' `stackPop` (numReturns (fs `getAtIndex` fIdx)))++(drop (numParams (fs `getAtIndex` fIdx)) s)) ls gs' fs is)

--(drop (numParams (fs `getAtIndex` fIdx)) s) this drops the number of input parameters we used from the original stack

{-Runs an if, first by checking top of stack for 0 or non-zero which will run is2 and i1 respectively
branchStep a WasmState which has the same locals and globals but different stack (stack with input parameters)
add final instruction onto the original instruction set (this is either nothing, Return, Br, or BrIf)
pop number of output results from the if onto the original stack-}
evalIf :: WasmState -> FuncType -> [Instr] -> [Instr] -> WasmState
evalIf (WasmState (0:s) ls gs fs is) (ps :-> rs) is1 is2 = case (branchSteps (WasmState (take (length ps) s) ls gs fs is2)) of
                                                              (WasmState s' ls' gs' fs' [Control (Br 0)]) -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs is)
                                                              --(WasmState s' ls' gs' fs' [Control (Br 0)]) -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs is)

                                                              (WasmState s' ls' gs' fs' [Control (Br n)]) -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs (Control (Br (n-1)):is))
                                                              --(WasmState s' ls' gs' fs' [Control (Br n)]) -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs (Control (Br (n-1)):is))

                                                              (WasmState s' ls' gs' fs' is') -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs (is'++is))
                                                              --(WasmState s' ls' gs' fs' is') -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs (is'++is))


evalIf (WasmState (_:s) ls gs fs is) (ps :-> rs) is1 is2 = case (branchSteps (WasmState (take (length ps) s) ls gs fs is1)) of
                                                              (WasmState s' ls' gs' fs' [Control (Br 0)]) -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs is)
                                                              --(WasmState s' ls' gs' fs' [Control (Br 0)]) -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs is)

                                                              (WasmState s' ls' gs' fs' [Control (Br n)]) -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs (Control (Br (n-1)):is))
                                                              --(WasmState s' ls' gs' fs' [Control (Br n)]) -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs (Control (Br (n-1)):is))

                                                              (WasmState s' ls' gs' fs' is') -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs (is'++is))
                                                              --(WasmState s' ls' gs' fs' is') -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs (is'++is))

--(drop (length ps) s) this drops the number of input parameters we used from the original stack

evalBlock :: WasmState -> FuncType -> [Instr] -> WasmState
evalBlock (WasmState s ls gs fs is) (ps :-> rs) is1 = case (branchSteps (WasmState (take (length ps) s) ls gs fs is1)) of
                                                          (WasmState s' ls' gs' fs' [Control (Br 0)]) -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs is)
                                                          --(WasmState s' ls' gs' fs' [Control (Br 0)]) -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs is)

                                                          (WasmState s' ls' gs' fs' [Control (Br n)]) -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs ((Control (Br (n-1))):is))
                                                          --(WasmState s' ls' gs' fs' [Control (Br n)]) -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs ((Control (Br (n-1))):is))

                                                          (WasmState s' ls' gs' fs' is') -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs (is'++is))
                                                          --(WasmState s' ls' gs' fs' is') -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs (is'++is))


evalLoop :: WasmState -> FuncType -> [Instr] -> WasmState
evalLoop (WasmState s ls gs fs is) (ps :-> rs) is1 = case (branchSteps (WasmState (take (length ps) s) ls gs fs is1)) of
                                                          (WasmState s' ls' gs' fs' [Control (Br 0)]) -> evalLoop (WasmState ((take (length rs) s')++(s)) ls' gs' fs is) (ps :-> rs) is1
                                                          --(WasmState s' ls' gs' fs' [Control (Br 0)]) -> evalLoop (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs is) (ps :-> rs) is1
                                                          --(WasmState s' ls' gs' fs' [Control (Br 0)]) -> evalLoop (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs is) (ps :-> rs) is1

                                                          (WasmState s' ls' gs' fs' [Control (Br n)]) -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs ((Control (Br (n-1))):is))
                                                          --(WasmState s' ls' gs' fs' [Control (Br n)]) -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs ((Control (Br (n-1))):is))

                                                          (WasmState s' ls' gs' fs' is') -> (WasmState ((take (length rs) s')++(drop (length ps) s)) ls' gs' fs (is'++is))
                                                          --(WasmState s' ls' gs' fs' is') -> (WasmState ((s' `stackPop` (length rs))++(drop (length ps) s)) ls' gs' fs (is'++is))


--Steps a branch until it reaches a return or runs out of instructions
branchSteps :: WasmState -> WasmState
branchSteps (WasmState s ls gs fs []) = (WasmState s ls gs fs [])
branchSteps (WasmState s ls gs fs ((Control Return):is)) = (WasmState s ls gs fs [Control Return])
branchSteps (WasmState s ls gs fs ((Control (Br n)):is)) = (WasmState s ls gs fs [Control (Br n)])
branchSteps (WasmState (0:s) ls gs fs [(Control (BrIf n))]) = (WasmState s ls gs fs [])
branchSteps (WasmState (0:s) ls gs fs ((Control (BrIf n)):is)) = branchSteps (step (WasmState s ls gs fs is)) --Top stack is 0 so no branch continue branchStep
--this is problematic what if loop has no more instructions
branchSteps (WasmState (_:s) ls gs fs ((Control (BrIf n)):is)) = (WasmState s ls gs fs [Control (Br n)])
branchSteps (WasmState s ls gs fs is) = branchSteps (step (WasmState s ls gs fs is))

numParams :: Function -> Int
numParams (Function (params :-> returns) ls is) = length params

numReturns :: Function -> Int
numReturns (Function (params :-> returns) ls is) = length returns

funcInstr :: Function -> [Instr]
funcInstr (Function fT ls is) = is

funcNumLTypes :: Function -> Int
funcNumLTypes (Function fT ls is) = length ls

--Pops number of elements and push onto another stack
stackPop :: [a] -> Int -> [a]
stackPop as n = reverse (take n as)

--Runs an unsigned operation as signed
signed :: (Int32 -> Int32 -> Int32) -> Word32 -> Word32 -> Word32
signed f w1 w2 = fromIntegral (f (fromIntegral w1) (fromIntegral w2))

--Signed shift right
signedSHR :: Int32 -> Int -> Word32
signedSHR i1 i2 = fromIntegral (shiftR i1 i2)

--Unsigned comparison
com :: (Word32 -> Word32 -> Bool) -> Word32 -> Word32 -> Word32
com f w1 w2
  | f w1 w2   = true
  | otherwise = false

--Signed comparison
signedCom :: (Int32 -> Int32 -> Bool) -> Word32 -> Word32 -> Word32
signedCom f w1 w2
  | f (fromIntegral w1) (fromIntegral w2)   = true
  | otherwise                               = false

--Boolean True represented as a 1
true :: Word32
true = 1

--Boolean False represented as a 0
false :: Word32
false = 0

--Wasm select instruction
select :: Word32 -> Word32 -> Word32 -> Word32
select w1 w2 w3
  | w1 == 0   = w2
  | otherwise = w3

--List getter
getAtIndex :: [a] -> Word32 -> a
getAtIndex (a:as) w
  | w==0      = a
  | otherwise = getAtIndex as (w-1)

--List setter
setAtIndex :: [a] -> Word32 -> a -> [a]
setAtIndex [] w a' = [a']
setAtIndex (a:as) w a'
  | w==0      = (a':as)
  | otherwise = a:(setAtIndex as (w-1) a')

--Method that steps until no instructions remain, returns the stack
steps :: WasmState -> [Word32]
steps (WasmState s _ _ _ []) = s
steps wS = steps (step wS)

--Method that runs a wasm instruction at a specific state
step :: WasmState -> WasmState
--Numerical Instructions
step (WasmState s ls gs fs ((Numeric I32 (Const i)):is)) = (WasmState (i:s) ls gs fs is)
step (WasmState (w:s)     ls gs fs ((Numeric I32 Clz):is))        = (WasmState (fromIntegral (countLeadingZeros w):s) ls gs fs is)
step (WasmState (w:s)     ls gs fs ((Numeric I32 Ctz):is))        = (WasmState (fromIntegral (countTrailingZeros w):s) ls gs fs is)
step (WasmState (w:s)     ls gs fs ((Numeric I32 Popcnt):is))     = (WasmState (fromIntegral (popCount w):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Add):is))        = (WasmState ((w1+w2):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Sub):is))        = (WasmState ((w2-w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Mul):is))        = (WasmState ((w1*w2):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Div S)):is))    = (WasmState ((signed div w2 w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Div U)):is))    = (WasmState ((w2 `div` w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Rem S)):is))    = (WasmState ((signed rem w2 w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Rem U)):is))    = (WasmState ((w2 `rem` w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Syntax.And):is)) = (WasmState ((w1 .&. w2):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Or):is))         = (WasmState ((w1 .|. w2):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Syntax.Xor):is)) = (WasmState ((w1 `xor` w2):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Shl):is))        = (WasmState ((w2 `shiftL` (fromIntegral w1)):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Shr S)):is))    = (WasmState (((fromIntegral w2) `signedSHR` (fromIntegral w1)):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Shr U)):is))    = (WasmState ((w2 `shiftR` (fromIntegral w1)):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Rotl):is))       = (WasmState ((w2 `rotateL` (fromIntegral w1)):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Rotr):is))       = (WasmState ((w2 `rotateR` (fromIntegral w1)):s) ls gs fs is)
step (WasmState (w:s)     ls gs fs ((Numeric I32 Eqz):is))        = (WasmState ((com (==) w 0):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Eq):is))         = (WasmState ((com (==) w1 w2):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 Ne):is))         = (WasmState ((com (/=) w1 w2):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Lt S)):is))     = (WasmState ((signedCom (<) w2 w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Lt U)):is))     = (WasmState ((com (<) w2 w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Gt S)):is))     = (WasmState ((signedCom (>) w2 w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Gt U)):is))     = (WasmState ((com (>) w2 w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Le S)):is))     = (WasmState ((signedCom (<=) w2 w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Le U)):is))     = (WasmState ((com (<=) w2 w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Ge S)):is))     = (WasmState ((signedCom (>=) w2 w1):s) ls gs fs is)
step (WasmState (w1:w2:s) ls gs fs ((Numeric I32 (Ge U)):is))     = (WasmState ((com (>=) w2 w1):s) ls gs fs is)

--Parameter Instructions
step (WasmState (w:s)         ls gs fs ((Param Drop):is))         = (WasmState s ls gs fs is)
step (WasmState (w1:w2:w3:s)  ls gs fs ((Param (Select I32)):is)) = (WasmState ((select w1 w2 w3):s) ls gs fs is)
--step (WasmState (0:w1:w2:s)  ls gs fs ((Param (Select I32)):is)) = (WasmState (w1:s) ls gs fs is) --probably more efficient
--step (WasmState (_:w1:w2:s)  ls gs fs ((Param (Select I32)):is)) = (WasmState (w2:s) ls gs fs is) --probably more efficient

--Variable Instructions
step (WasmState s     ls gs fs ((Variable (LocalGet idx)):is))    = (WasmState ((ls `getAtIndex` idx):s) ls gs fs is)
step (WasmState (w:s) ls gs fs ((Variable (LocalSet idx)):is))    = (WasmState s (setAtIndex ls idx w) gs fs is)
step (WasmState (w:s) ls gs fs ((Variable (LocalTee idx)):is))    = (WasmState (w:s) (setAtIndex ls idx w) gs fs is)
step (WasmState s     ls gs fs ((Variable (GlobalGet idx)):is))   = (WasmState ((gs `getAtIndex` idx):s) ls gs fs is)
step (WasmState (w:s) ls gs fs ((Variable (GlobalSet idx)):is))   = (WasmState s ls (setAtIndex gs idx w) fs is)

--Control Instructions
step (WasmState s ls gs fs ((Control Nop):is))                    = (WasmState s ls gs fs is)
step (WasmState s ls gs fs ((Control (Call fIdx)):is))            = evalFunc (WasmState s ls gs fs is) fIdx
step (WasmState s ls gs fs ((Control Return):is))                 = error "some how ran into a rogue return" --this should be happening under branchStep
step (WasmState s ls gs fs ((Control (If fT is1 is2)):is))        = evalIf (WasmState s ls gs fs is) fT is1 is2

--Extra credit

--Block, Loop, Br, BrIf
step (WasmState s ls gs fs ((Control (Block fT is')):is)) = evalBlock (WasmState s ls gs fs is) fT is'
step (WasmState s ls gs fs ((Control (Loop fT is')):is)) = evalLoop (WasmState s ls gs fs is) fT is'
step (WasmState s ls gs fs ((Control (Br lIdx)):is)) = error "some how ran into a rogue Br" --this should be happening under branchStep
step (WasmState s ls gs fs ((Control (BrIf lIdx)):is)) = error "some how ran into a rogue BrIf" --this should be happening under branchStep

--Floating Point Numbers
--step w = trace (show w) w
step _ = undefined

----------------------------------------------------------------------------
--Messing with Haskell Data.Binary put/get
--import Data.Binary
--Can't for some reason

--import GHC.Float

--castWord32ToFloat :: Word -> Float

--castFloatToWord32 :: Float -> Word32

floatOp :: (Float -> Float -> Float) -> Word32 -> Word32 -> Word32
floatOp f w1 w2 = castFloatToWord32(f (castWord32ToFloat w1) (castWord32ToFloat w2))

roundF :: Word32 -> Int
roundF w = round (castWord32ToFloat w)

floatCom :: (Float -> Float -> Bool) -> Word32 -> Word32 -> Word32
floatCom f w1 w2
  | f (castWord32ToFloat w1) (castWord32ToFloat w2) = true
  | otherwise = false

--{-



floatStep :: WasmState -> WasmState
floatStep (WasmState s ls gs fs ((Numeric F32 (Const i)):is)) = (WasmState (i:s) ls gs fs is)
floatStep (WasmState (w:s)     ls gs fs ((Numeric F32 Clz):is))        = (WasmState (fromIntegral (countLeadingZeros w):s) ls gs fs is)
floatStep (WasmState (w:s)     ls gs fs ((Numeric F32 Ctz):is))        = (WasmState (fromIntegral (countTrailingZeros w):s) ls gs fs is)
floatStep (WasmState (w:s)     ls gs fs ((Numeric F32 Popcnt):is))     = (WasmState (fromIntegral (popCount (complement w)):s) ls gs fs is)

floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Add):is))        = (WasmState ((floatOp plusFloat w1 w2):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Sub):is))        = (WasmState ((floatOp minusFloat w2 w1):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Mul):is))        = (WasmState ((floatOp timesFloat w1 w2):s) ls gs fs is)

floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 (Div _)):is))    = (WasmState ((floatOp divideFloat w2 w1):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 (Rem _)):is))    = (WasmState ((floatOp (mod') w2 w1):s) ls gs fs is)

--Bit Operations stay the same
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Syntax.And):is)) = (WasmState ((w1 .&. w2):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Or):is))         = (WasmState ((w1 .|. w2):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Syntax.Xor):is)) = (WasmState ((w1 `xor` w2):s) ls gs fs is)

--Need to round float before shifting/rotating, can't do those operations with non-whole numbers
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Shl):is))        = (WasmState ((w2 `shiftL` (roundF w1)):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 (Shr S)):is))    = (WasmState (((fromIntegral w2) `signedSHR` (roundF w1)):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 (Shr U)):is))    = (WasmState ((w2 `shiftR` (roundF w1)):s) ls gs fs is) 
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Rotl):is))       = (WasmState ((w2 `rotateL` (roundF w1)):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Rotr):is))       = (WasmState ((w2 `rotateR` (roundF w1)):s) ls gs fs is)

floatStep (WasmState (w:s)     ls gs fs ((Numeric F32 Eqz):is))        = (WasmState ((floatCom (==) w 0):s) ls gs fs is)
--for float -0 = 0
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Eq):is))         = (WasmState ((floatCom (==) w1 w2):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 Ne):is))         = (WasmState ((floatCom (/=) w1 w2):s) ls gs fs is)

--Floats are signed by default
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 (Lt _)):is))     = (WasmState ((floatCom (<) w2 w1):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 (Gt _)):is))     = (WasmState ((floatCom (>) w2 w1):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 (Le _)):is))     = (WasmState ((floatCom (<=) w2 w1):s) ls gs fs is)
floatStep (WasmState (w1:w2:s) ls gs fs ((Numeric F32 (Ge _)):is))     = (WasmState ((floatCom (>=) w2 w1):s) ls gs fs is)

floatStep (WasmState (w1:w2:w3:s)  ls gs fs ((Param (Select F32)):is)) 
  | (w1==0) = (WasmState (w2:s) ls gs fs is)
  | (isNegativeZero (castWord32ToFloat w1)) = (WasmState (w2:s) ls gs fs is)
  | otherwise = (WasmState (w3:s) ls gs fs is)

--floatStep wState = step wState
floatStep (WasmState s ls gs fs is) = undefined

-- -}
-----------------------------------------------------------------------------------------------------
--Debugging collatz_sum.wast
--collatz :: Module
--collatz = Module [Function {funcType = [I32] :-> [I32,I32], locals = [], body = [Variable (LocalGet 0),Variable (LocalGet 0)]},Function {funcType = [I32] :-> [I32], locals = [], body = [Variable (LocalGet 0),Numeric I32 (Const 2),Numeric I32 (Rem U),Numeric I32 Eqz,Control (If ([] :-> [I32]) [Variable (LocalGet 0),Numeric I32 (Const 2),Numeric I32 (Div U)] [Variable (LocalGet 0),Numeric I32 (Const 3),Numeric I32 Mul,Numeric I32 (Const 1),Numeric I32 Add])]},Function {funcType = [I32] :-> [I32], locals = [I32], body = [Variable (LocalGet 0),Variable (LocalSet 1),Control (Loop ([] :-> [I32]) [Variable (LocalGet 0),Control (Call 1),Variable (LocalTee 0),Variable (LocalGet 1),Numeric I32 Add,Variable (LocalSet 1),Variable (LocalGet 0),Numeric I32 (Const 1),Numeric I32 Ne,Control (BrIf 0)]),Variable (LocalGet 1)]},Function {funcType = [I32] :-> [I32], locals = [], body = [Variable (LocalGet 0),Control (Call 2)]}], types = [[] :-> [I32],[I32] :-> [I32],[I32] :-> [I32,I32]], globals = [], start = 3
f0 :: Function
f0 = Function ([I32] :-> [I32,I32]) [] b0
b0 :: [Instr]
b0 = [] --unused

f1 :: Function
f1 = Function ([I32] :-> [I32]) [] b1
b1 :: [Instr]
b1 = [Variable (LocalGet 0),Numeric I32 (Const 2),Numeric I32 (Rem U),Numeric I32 Eqz,Control (If ([] :-> [I32]) ifInstr1 ifInstr2)]
ifInstr1 :: [Instr]
ifInstr1 = [Variable (LocalGet 0),Numeric I32 (Const 2),Numeric I32 (Div U)]
ifInstr2 :: [Instr]
ifInstr2 = [Variable (LocalGet 0),Numeric I32 (Const 3),Numeric I32 Mul,Numeric I32 (Const 1),Numeric I32 Add]

f2 :: Function
f2 = Function ([I32] :-> [I32]) [I32] b2
b2 :: [Instr]
b2 = [Variable (LocalGet 0),Variable (LocalSet 1),Control (Loop ([] :-> [I32]) loopInstr),Variable (LocalGet 1)]
loopInstr :: [Instr]
loopInstr = [Variable (LocalGet 0),Control (Call 1),Variable (LocalTee 0),Variable (LocalGet 1),Numeric I32 Add,Variable (LocalSet 1),Variable (LocalGet 0),Numeric I32 (Const 1),Numeric I32 Ne,Control (BrIf 0)]

f3 :: Function
f3 = Function ([I32] :-> [I32]) [] b3
b3 :: [Instr]
b3 = [Variable (LocalGet 0),Control (Call 2)]

fs :: [Function]
fs = [f0,f1,f2,f3]

ts :: [FuncType]
ts = [[] :-> [I32],[I32] :-> [I32],[I32] :-> [I32,I32]]

gs :: [([Instr], ValType)]
gs = []

s :: FuncIdx
s = 3

m :: Module
m = Module fs ts gs s

input :: [Word32]
input = [7]
{-
Module {functions = [
Function {funcType = [I32] :-> [I32,I32], locals = [], 
body = [Variable (LocalGet 0),Variable (LocalGet 0)]}, --unused

Function {funcType = [I32] :-> [I32], locals = [], 
body = [Variable (LocalGet 0),Numeric I32 (Const 2),Numeric I32 (Rem U),Numeric I32 Eqz,Control (If ([] :-> [I32]) [Variable (LocalGet 0),Numeric I32 (Const 2),Numeric I32 (Div U)] [Variable (LocalGet 0),Numeric I32 (Const 3),Numeric I32 Mul,Numeric I32 (Const 1),Numeric I32 Add])]},

Function {funcType = [I32] :-> [I32], locals = [I32], 
body = [Variable (LocalGet 0),Variable (LocalSet 1),Control (Loop ([] :-> [I32]) [Variable (LocalGet 0),Control (Call 1),Variable (LocalTee 0),Variable (LocalGet 1),Numeric I32 Add,Variable (LocalSet 1),Variable (LocalGet 0),Numeric I32 (Const 1),Numeric I32 Ne,Control (BrIf 0)]),Variable (LocalGet 1)]},

Function {funcType = [I32] :-> [I32], locals = [], 
body = [Variable (LocalGet 0),Control (Call 2)]}], 

types = [[] :-> [I32],[I32] :-> [I32],[I32] :-> [I32,I32]], 
globals = [], 
start = 3}








-}