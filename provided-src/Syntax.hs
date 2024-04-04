module Syntax where

import Data.Word

type Value = Word32
  -- This type could be extended, if you were implementing extra data types as extra credit.

data ValType = I32 | F32
  deriving (Eq, Show)

data FuncType = 
  [ValType] :-> [ValType]
  deriving (Eq, Show)

type TypeIdx = Word32

---------------------------------------------------------------------------------
-- Instructions come in four categories:
--
-- Numeric operations.  These all come with a tag indicating what *size* operand
-- they work on; since we're only implementing `i32`, this will always be I32.
--
-- Parametric operations.  These manipulate the stack.
--
-- Variable operations.  These read and write local and global variables.
--
-- Control operations.  These provide flow of control.

data Instr =
    Numeric ValType NumOp  
  | Param ParamOp
  | Variable VarOp
  | Control ConOp
  deriving (Eq, Show)

data Signed = S | U deriving (Eq, Show)

data NumOp =
    Const Value
  | Clz         -- Count leading zeros --- in the binary representation of the argument
  | Ctz         -- Count trailing zeros --- in the binary representation of the argument
  | Popcnt      -- Count all zeros --- in the binary representation of the argument
  -- Typical numeric operations
  | Add | Sub | Mul 
  -- Division can be either signed or unsigned
  | Div Signed | Rem Signed
  -- Bitwise operations
  | And | Or | Xor 
  -- The right-shift needs to be signed or unsigned (to get sign extension right)
  | Shl | Shr Signed | Rotl | Rotr  
  | Eqz         -- Is the argument zero?
  | Eq | Ne 
  -- The ordering operations need to know if their arguments are signed or unsigned.
  | Lt Signed | Gt Signed | Le Signed | Ge Signed
  deriving (Eq, Show)

data ParamOp =
    Drop              -- Drops the top value on the stack
  | Select ValType    -- Pops 3 values; if the first was 0, pushes the second; 
                      -- otherwise, pushes the third
  deriving (Eq, Show)

type LocalIdx = Word32
type GlobalIdx = Word32

data VarOp =
    LocalGet LocalIdx   -- Pushes the value of a local variable onto the stack
  | LocalSet LocalIdx   -- Pops the value on top of the stack and writes it to a local variable
  | LocalTee LocalIdx   -- Writes the top value on the stack to a local variable, 
                        -- *without* popping the value off the stack
  | GlobalGet GlobalIdx -- As above, but for globals
  | GlobalSet GlobalIdx -- As above, but for globals
  deriving (Eq, Show)

type LabelIdx = Word32
type FuncIdx  = Word32

data ConOp =
    Nop                 -- Does nothing
  | Block FuncType [Instr]
                        -- Executes the instructions in the instruction list
  | Loop FuncType [Instr]
                        -- Executes the instructions in the instruction list; the difference 
                        -- between `loop` and `block` is how `br` and `br_if` treat them,
  | If FuncType [Instr] [Instr]
                        -- Pops the top value from the stack.  If that value is non-zero, executes
                        -- the first list of instructions; otherwise, the second.
  | Br LabelIdx         -- `Br n` jumps to the n'th surrounding block
  | BrIf LabelIdx       -- Pops the top value from the stack.  If that value is non-zero, jumps to
                        -- the n'th surrounding block
  | Return              -- Returns from a function call
  | Call FuncIdx        -- Calls a function
  deriving (Eq, Show)
  
data Module = 
  Module { functions :: [Function]
         , types :: [FuncType] 
         , globals :: [([Instr], ValType)] -- globals and their initializers
         , start :: FuncIdx }
  deriving (Eq, Show)

data Function =                      
  Function { funcType :: FuncType
           , locals :: [ValType]
           , body :: [Instr] }
  deriving (Eq, Show)

