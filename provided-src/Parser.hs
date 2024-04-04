module Parser where

import Syntax
import Text.Parsec as Parsec
import Data.Char as Char hiding (Control)
import Data.Word
import System.IO.Error

type Thing u a = Parsec.Parsec String u a

data PvtInstr = 
    PvtControl PvtConOp 
  | Instr Instr
  deriving (Eq, Show)

data PvtConOp = 
    PvtBlock TypeIdx [PvtInstr]
  | PvtLoop TypeIdx [PvtInstr]
  | PvtIf TypeIdx [PvtInstr] [PvtInstr]
  deriving (Eq, Show)
  
data PvtFunction = PvtFunction
  { pvtFuncType :: TypeIdx
  , pvtLocals :: [ValType]
  , pvtBody :: [PvtInstr]
  }
  deriving (Eq, Show)

data Field =
    PvtFField PvtFunction
  | FField Function
  | TField FuncType
  | GField [Instr] ValType
  | SField FuncIdx
  deriving (Show)

lParen :: Char
lParen = '('

rParen :: Char
rParen = ')'

semicolon :: Char
semicolon = ';'

parseLParen :: Thing u Char
parseLParen = Parsec.char lParen

parseRParen :: Thing u Char
parseRParen = Parsec.char rParen

parseSemicolon :: Thing u Char
parseSemicolon = Parsec.char semicolon

parseSpace :: Thing u String
parseSpace = Parsec.string " " <|> 
             parseFormat       <|>
             parseLineComment

parseFormat :: Thing u String
parseFormat = (: []) <$> (parseNewline <|> parseTab)

parseNewline :: Thing u Char
parseNewline = Parsec.char '\n' <|> Parsec.char '\r'

parseTab :: Thing u Char
parseTab = Parsec.char '\t'
    
parseLineComment :: Thing u String
parseLineComment = do
  Parsec.try (do parseSemicolon; parseSemicolon)
  cs <- Parsec.many (Parsec.noneOf ['\n', '\r'])
  skipNewline <|> Parsec.eof
  return cs
    where
  skipNewline = do parseNewline; return ()

groupFields :: [Field] 
            -> ([PvtFunction], [FuncType], [([Instr], ValType)], FuncIdx)
            -> ([PvtFunction], [FuncType], [([Instr], ValType)], FuncIdx)
groupFields []                (pvtFs, ts, gs, st) = 
  (reverse pvtFs, reverse ts, reverse gs, st)
groupFields (PvtFField pvtF : rest) (pvtFs, ts, gs, st) =
  groupFields rest (pvtF : pvtFs, ts, gs, st) 
groupFields (FField _ : rest) (pvtFs, ts, gs, st) =
  groupFields rest (pvtFs, ts, gs, st) 
groupFields (TField t : rest) (pvtFs, ts, gs, st) =
  groupFields rest (pvtFs, t : ts, gs, st)
groupFields (GField instrs t : rest) (pvtFs, ts, gs, st) =
  groupFields rest (pvtFs, ts, (instrs, t) : gs, st)
groupFields (SField st : rest) (pvtFs, ts, gs, _) =
  groupFields rest (pvtFs, ts, gs, st)

parseModule :: Thing u Module
parseModule = Parsec.between parseLParen parseRParen $ do
  Parsec.string "module"
  Parsec.many parseSpace 
  fields <- parseField `Parsec.sepBy` Parsec.many parseSpace
  let (pvtFs, ts, gs, st) = groupFields fields ([], [], [], 0)
  let fs = map (pvtFunctionToFunction ts) pvtFs
  return (Module fs ts gs st)

pvtFunctionToFunction :: [FuncType] -> PvtFunction -> Function
pvtFunctionToFunction ts pvtF = Function
  { funcType = ts !! fromEnum (pvtFuncType pvtF)
  , locals = pvtLocals pvtF
  , body = map (pvtInstrToInstr ts) (pvtBody pvtF)
  }

pvtInstrToInstr :: [FuncType] -> PvtInstr -> Instr
pvtInstrToInstr ts (PvtControl (PvtBlock typeIdx instrs)) = 
  Control (Block funcType instrs')
    where
  funcType = ts !! fromEnum typeIdx
  instrs' = map (pvtInstrToInstr ts) instrs
pvtInstrToInstr ts (PvtControl (PvtLoop typeIdx instrs)) =
  Control (Loop funcType instrs')
    where
  funcType = ts !! fromEnum typeIdx
  instrs' = map (pvtInstrToInstr ts) instrs
pvtInstrToInstr ts (PvtControl (PvtIf typeIdx thenInstrs elseInstrs)) = 
  Control (If funcType thenInstrs' elseInstrs')
    where
  funcType = ts !! fromEnum typeIdx
  thenInstrs' = map (pvtInstrToInstr ts) thenInstrs
  elseInstrs' =  map (pvtInstrToInstr ts) elseInstrs
pvtInstrToInstr ts (Instr instr) = instr

parseField :: Thing u Field
parseField = Parsec.between
  parseLParen
  parseRParen
  (    parsePvtFunction
   <|> parseType
   <|> parseGlobal
   <|> parseStart)

parsePvtFunction :: Thing u Field
parsePvtFunction = do
  Parsec.string "func"
  Parsec.many parseSpace 
  t <- parseTypeUse
  Parsec.many parseSpace
  ls <- Parsec.many parseLocal
  Parsec.many parseSpace
  b <- parseInstr `Parsec.sepBy` Parsec.many1 parseSpace
  return $ PvtFField (PvtFunction t ls b)

parseTypeUse :: Thing u TypeIdx
parseTypeUse = Parsec.between parseLParen parseRParen $ do
  Parsec.string "type"
  Parsec.many1 parseSpace
  parseU32  

parseLocal :: Thing u ValType
parseLocal = Parsec.between parseLParen parseRParen $ do
  Parsec.string "local"
  Parsec.many1 parseSpace
  parseValType  

parseNop :: Thing u ConOp
parseNop = do
  Parsec.string "nop"
  return Nop

parseCall :: Thing u ConOp
parseCall = do
  Parsec.string "call"
  Parsec.many1 parseSpace
  funcIdx <- parseU32
  return (Call funcIdx)

parseLocalGet :: Thing u VarOp
parseLocalGet = do
  Parsec.string "local.get"
  Parsec.many1 parseSpace
  localIdx <- parseU32
  return (LocalGet localIdx)

parseLocalSet :: Thing u VarOp
parseLocalSet = do
  Parsec.string "local.set"
  Parsec.many1 parseSpace
  localIdx <- parseU32
  return (LocalSet localIdx)

parseLocalTee :: Thing u VarOp
parseLocalTee = do
  Parsec.string "local.tee"
  Parsec.many1 parseSpace
  localIdx <- parseU32
  return (LocalTee localIdx)

parseGlobalGet :: Thing u VarOp
parseGlobalGet = do
  Parsec.string "global.get"
  Parsec.many1 parseSpace
  globalIdx <- parseU32
  return (GlobalGet globalIdx)

parseGlobalSet :: Thing u VarOp
parseGlobalSet = do
  Parsec.string "global.set"
  Parsec.many1 parseSpace
  globalIdx <- parseU32
  return (GlobalSet globalIdx)

parseClz :: Thing u NumOp
parseClz = do
  Parsec.string "clz"
  return Clz

parseCtz :: Thing u NumOp
parseCtz = do
  Parsec.string "ctz"
  return Ctz

parsePopcnt :: Thing u NumOp
parsePopcnt = do
  Parsec.string "popcnt"
  return Popcnt

parseAdd :: Thing u NumOp
parseAdd = do
  Parsec.string "add"
  return Add

parseSub :: Thing u NumOp
parseSub = do
  Parsec.string "sub"
  return Sub

parseMul :: Thing u NumOp
parseMul = do
  Parsec.string "mul"
  return Mul

parseDiv :: Thing u NumOp
parseDiv = do
  Parsec.string "div_"
  signed <- parseSigned
  return (Div signed)

parseSigned :: Thing u Signed
parseSigned = (do Parsec.char 's'; return S)
          <|> (do Parsec.char 'u'; return U)

parseRem :: Thing u NumOp
parseRem = do
  Parsec.string "rem_"
  signed <- parseSigned
  return (Rem signed)

parseAnd :: Thing u NumOp
parseAnd = do
  Parsec.string "and"
  return And

parseOr :: Thing u NumOp
parseOr = do
  Parsec.string "or"
  return Or

parseXor :: Thing u NumOp
parseXor = do
  Parsec.string "xor"
  return Xor

parseShl :: Thing u NumOp
parseShl = do
  Parsec.string "shl"
  return Shl

parseShr :: Thing u NumOp
parseShr = do
  Parsec.string "shr_"
  signed <- parseSigned
  return (Shr signed)

parseRotl :: Thing u NumOp
parseRotl = do
  Parsec.string "rotl"
  return Rotl

parseRotr :: Thing u NumOp
parseRotr = do
  Parsec.string "rotr"
  return Rotr

parseEqz :: Thing u NumOp
parseEqz = do
  Parsec.string "eqz"
  return Eqz

parseEq :: Thing u NumOp
parseEq = do
  Parsec.string "eq"
  return Eq

parseNe :: Thing u NumOp
parseNe = do
  Parsec.string "ne"
  return Ne

parseLt :: Thing u NumOp
parseLt = do
  Parsec.string "lt_"
  signed <- parseSigned
  return (Lt signed)

parseGt :: Thing u NumOp
parseGt = do
  Parsec.string "gt_"
  signed <- parseSigned
  return (Gt signed)

parseLe :: Thing u NumOp
parseLe = do
  Parsec.string "le_"
  signed <- parseSigned
  return (Le signed)

parseGe :: Thing u NumOp
parseGe = do
  Parsec.string "ge_"
  signed <- parseSigned
  return (Ge signed)

parseConst :: Thing u NumOp
parseConst = do
  Parsec.string "const"
  Parsec.many1 parseSpace
  value <- parseValue
  return (Const value)

parseBlockInstrs :: [PvtInstr] -> String -> Thing u [PvtInstr]
parseBlockInstrs acc delim = let
  goOn = do
    instr <- try parseInstr
    parseBlockInstrs (instr : acc) delim
  halt = do
    Parsec.string delim
    return (reverse acc)
      in do
  Parsec.many1 parseSpace
  goOn <|> halt

parsePvtBlock :: Thing u PvtConOp
parsePvtBlock = do
  Parsec.string "block"
  Parsec.many1 parseSpace
  typeIdx <- parseTypeUse
  instrs <- parseBlockInstrs [] "end"
  return (PvtBlock typeIdx instrs)

parsePvtLoop :: Thing u PvtConOp
parsePvtLoop = do
  Parsec.string "loop"
  Parsec.many1 parseSpace
  typeIdx <- parseTypeUse
  instrs <- parseBlockInstrs [] "end"
  return (PvtLoop typeIdx instrs)

parsePvtIf :: Thing u PvtConOp
parsePvtIf = do
  Parsec.string "if"
  Parsec.many1 parseSpace
  typeIdx <- parseTypeUse
  thenInstrs <- parseBlockInstrs [] "else"
  elseInstrs <- parseBlockInstrs [] "end"
  return (PvtIf typeIdx thenInstrs elseInstrs)

parseBr :: Thing u ConOp
parseBr = do
  Parsec.string "br"
  Parsec.many1 parseSpace
  labelIdx <- parseU32
  return (Br labelIdx)

parseBrIf :: Thing u ConOp
parseBrIf = do
  Parsec.string "br_if"
  Parsec.many1 parseSpace
  labelIdx <- parseU32
  return (BrIf labelIdx)

parseReturn :: Thing u ConOp
parseReturn = do
  Parsec.string "return"
  return Return

parseInstr :: Thing u PvtInstr
parseInstr = Parsec.try (Instr . Control  <$> parseNop)
         <|> Parsec.try (Instr . Control  <$> parseCall)
         <|> Parsec.try (PvtControl  <$> parsePvtBlock)
         <|> Parsec.try (PvtControl  <$> parsePvtLoop)
         <|> Parsec.try (PvtControl  <$> parsePvtIf)
         <|> Parsec.try (Instr . Control  <$> parseBr)
         <|> Parsec.try (Instr . Control  <$> parseBrIf)
         <|> Parsec.try (Instr . Control  <$> parseReturn)
         <|> Parsec.try (Instr . Variable <$> parseLocalGet)
         <|> Parsec.try (Instr . Variable <$> parseLocalSet)
         <|> Parsec.try (Instr . Variable <$> parseLocalTee)
         <|> Parsec.try (Instr . Variable <$> parseGlobalGet)
         <|> Parsec.try (Instr . Variable <$> parseGlobalSet)
         <|> parseNumeric
   
parseNumeric :: Thing u PvtInstr
parseNumeric = do
  valType <- try parseValType
  Parsec.char '.'
  numOp <- parseNumOp
  return (Instr (Numeric valType numOp))

parseNumOp :: Thing u NumOp
parseNumOp = Parsec.try parseClz
         <|> Parsec.try parseCtz
         <|> Parsec.try parsePopcnt
         <|> Parsec.try parseAdd
         <|> Parsec.try parseSub
         <|> Parsec.try parseMul
         <|> Parsec.try parseDiv
         <|> Parsec.try parseRem
         <|> Parsec.try parseAnd
         <|> Parsec.try parseOr
         <|> Parsec.try parseXor
         <|> Parsec.try parseShl
         <|> Parsec.try parseShr
         <|> Parsec.try parseRotl
         <|> Parsec.try parseRotr
         <|> Parsec.try parseEqz
         <|> Parsec.try parseEq
         <|> Parsec.try parseNe
         <|> Parsec.try parseLt
         <|> Parsec.try parseGt
         <|> Parsec.try parseLe
         <|> Parsec.try parseGe
         <|> parseConst

parseType :: Thing u Field
parseType = do
  Parsec.string "type"
  Parsec.many parseSpace
  TField <$> parseFuncType
  
parseFuncType :: Thing u FuncType
parseFuncType = Parsec.between parseLParen parseRParen $ do
  Parsec.string "func"
  Parsec.many parseSpace
  ins <- parseParams
  Parsec.many parseSpace
  outs <- parseResult `Parsec.sepBy` Parsec.many parseSpace
  return (ins :-> outs)

parseParams :: Thing u [ValType]
parseParams = do
  maybeValType <- Parsec.optionMaybe (Parsec.try parseParam)
  case maybeValType of
    Nothing    -> 
      return []
    Just valType -> do
      Parsec.many parseSpace
      rest <- parseParams
      return (valType : rest)

parseParam :: Thing u ValType
parseParam = Parsec.between parseLParen parseRParen $ do
  Parsec.string "param"
  Parsec.many1 parseSpace
  parseValType

parseResult :: Thing u ValType
parseResult = Parsec.between parseLParen parseRParen $ do
  Parsec.string "result"
  Parsec.many1 parseSpace
  parseValType

parseValType :: Thing u ValType
parseValType = do
  Parsec.string "i32"
  return I32
  
parseGlobal :: Thing u Field
parseGlobal = do
  Parsec.string "global"
  Parsec.many1 parseSpace
  t <- parseGlobalType
  parseSpacesInstrs t <|> return (GField [] t)
    where
  parseSpacesInstrs t = do
    Parsec.many1 parseSpace
    instrs <- parseInstr `Parsec.sepBy` Parsec.many1 parseSpace
    return (GField (map (pvtInstrToInstr []) instrs) t)

parseGlobalType :: Thing u ValType
parseGlobalType = parseValType

parseValue :: Thing u Value
parseValue = parseU32

parseStart :: Thing u Field
parseStart = do
  Parsec.string "start"
  many1 parseSpace
  SField <$> parseU32

parseU32 :: Thing u Word32
parseU32 = read <$> (parse0xHexnum <|> parseNum)

parse0xHexnum :: Thing u String
parse0xHexnum = do
  Parsec.try (Parsec.string "0x")
  ds <- Parsec.many1 Parsec.hexDigit
  return $ "0x" ++ ds

parseNum :: Thing u String
parseNum = Parsec.many1 Parsec.digit

readModule :: FilePath -> IO Module
readModule source = 
  do moduleOrError <- Parsec.parse parseModule source <$> readFile source
     case moduleOrError of
       Left parseError ->
         ioError (userError (show parseError))
       Right mod ->
         return mod

makeTest :: FilePath -> (Module -> Bool) -> IO Bool
makeTest source predicate = do
  moduleOrError <- Parsec.parse parseModule source <$> readFile source
  case moduleOrError of
    Left parseError -> do
      putStrLn (show parseError)
      return False
    Right module' -> do
      return (predicate module')

test1 :: IO Bool
test1 = makeTest "provided-src/test1" (m ==)
    where
  m = Module
    { functions = []
    , types = []
    , globals = []
    , start = 0
    }

test2 :: IO Bool
test2 = makeTest "provided-src/test2" (m ==)
    where
  m = Module
    { functions = 
        [ Function
            { funcType = [I32, I32] :-> [I32]
            , locals = []
            , body =
                [ Variable (LocalGet 0)
                , Variable (LocalGet 1)
                , Numeric I32 Add
                ]
            }
        ]
    , types = [ [I32, I32] :-> [I32] ]
    , globals = []
    , start = 0
    }

test3 :: IO Bool
test3 = makeTest "provided-src/test3" (m ==)
    where
  m = Module
    { functions =
        [ Function
            { funcType = [] :-> [I32]
            , locals = []
            , body =
                [ Numeric I32 (Const 42)
                ]
            }
        , Function
            { funcType = [] :-> [I32]
            , locals = []
            , body = 
                [ Control (Call 0)
                , Numeric I32 (Const 1)
                , Numeric I32 Add
                ]
            }
        , Function
            { funcType = [I32, I32] :-> []
            , locals = [I32]
            , body =
                [ Variable (LocalGet 0)
                , Variable (LocalGet 1)
                , Variable (LocalGet 2)
                ]
            }
        ]
    , types =
        [ [] :-> [I32]
        , [I32, I32] :-> []
        ]
    , globals = 
        [ ([], I32) 
        ]
    , start = 0
    }

test4 :: IO Bool
test4 = makeTest "provided-src/test4" (m ==)
    where
  m = Module
    { functions = 
        [ Function
            { funcType = [I32] :-> []
            , locals = []
            , body =
                [ Control $ Block ([] :-> [])
                    [ Variable (LocalGet 0)
                    , Numeric I32 (Const 100)
                    , Numeric I32 Eq
                    , Variable (LocalGet 0)
                    ]
                ]
            }
        ]
    , types = 
        [ [I32] :-> []
        , [] :-> []
        ]
    , globals = []
    , start = 0
    }
