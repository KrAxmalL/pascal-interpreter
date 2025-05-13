module Lexic where

import Text.Parsec (SourcePos)

-- https://www.freepascal.org/docs-html/current/ref/refse111.html#x232-25600016.1
data Program = Program { pHeader :: Identifier, pBody :: Block, pPos :: SourcePos} deriving (Show)

-- https://www.freepascal.org/docs-html/current/ref/refse115.html#x236-26000016.5
data Block = Block {bDeclarations :: [Declaration], bBody :: Statement} deriving (Show)

data Declaration = VarDecl [Variable] | FuncDecl Function | ProcDecl Procedure deriving (Show)

-- https://www.freepascal.org/docs-html/current/ref/refse22.html
data Variable = Var { vName :: Identifier, vType :: DataType, vValue :: Maybe Value } deriving (Show)

-- https://www.freepascal.org/docs-html/current/ref/refse93.html#x177-20100014.2
data Function = Function {fName :: Identifier, fParams :: [FormalParam], fResType :: DataType, fBlock :: Block} deriving (Show)

-- https://www.freepascal.org/docs-html/current/ref/refse92.html#x176-20000014.1
data Procedure = Procedure {pName :: Identifier, pParams :: [FormalParam], pBlock :: Block} deriving (Show)

data FormalParam = FormalParam {fpName :: Identifier, fpType :: DataType} deriving (Show)

-- TODO: check the cases when:
-- 1) function or procedure defined without parameters and parenthesis
-- 2) function or procedure called without parameters and parenthesis

-- https://www.freepascal.org/docs-html/current/ref/refch13.html#x159-18300013
data Statement = Assignment {aName :: Identifier, aValue :: Expression } |
                 ProcCall {pcName :: Identifier, pcParams :: [Expression]} |
                 Compound [Statement] |
                 If { iCondition :: Expression, iIfRoute :: Statement, iElseRoute :: Maybe Statement } |
                 While { wCondition :: Expression, wBody :: Statement } deriving (Show)


-- https://www.freepascal.org/docs-html/current/ref/refch12.html#x142-16600012
data Expression = Val Value |
                  VarRef Identifier |
                  FuncCall {fcName :: Identifier, fcParams :: [Expression]} |
                  UnOp UnaryOp Expression |
                  BinOp {boOp :: BinaryOp, boLeft :: Expression, boRight :: Expression} |
                  Paren Expression
                  deriving (Show)

data UnaryOp = Not |
               UnaryPlus |
               UnaryMinus
               deriving (Show, Eq)

data BinaryOp = Plus |
                Minus |
                Mul |
                Div |
                FullDiv |
                Mod |
                Eql |
                Neql |
                Gt |
                Gte |
                Lt |
                Lte |
                And |
                Or |
                Xor
                deriving (Show, Eq)


-- https://www.freepascal.org/docs-html/current/ref/refse4.html#x15-140001.4
data Identifier = Identifier {idValue :: String}
    deriving (Show)

data DataType = DTInteger |
                DTBoolean
                deriving (Show, Eq)

data Value = Boolean Bool |
             IntNum Int
             deriving (Show)

printValue :: Value -> String
printValue (Boolean v) = show v
printValue (IntNum v) = show v

-- TODO: 
-- 1. Rework type checking in interpreter to have a Map {operator -> list of allowed data type pairs} for improved readability
-- 2. Consider supporting Real data type
-- 3. Consider adding more control flow statements (repeat..., for...)
-- 5. Add readln function support