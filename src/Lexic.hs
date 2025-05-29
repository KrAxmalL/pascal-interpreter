module Lexic where

data Program = Program {pHeader :: Identifier, pBody :: Block} deriving (Show)

data Block = Block {bDeclarations :: [Declaration], bBody :: Statement} deriving (Show)

data Declaration = VarDecl [Variable] | FuncDecl Function | ProcDecl Procedure deriving (Show)

data Variable = Var {vName :: Identifier, vType :: DataType, vValue :: Maybe Value} deriving (Show)

data Function = Function {fName :: Identifier, fParams :: [FormalParam], fResType :: DataType, fBlock :: Block} deriving (Show)

data Procedure = Procedure {pName :: Identifier, pParams :: [FormalParam], pBlock :: Block} deriving (Show)

data FormalParam = FormalParam {fpName :: Identifier, fpType :: DataType} deriving (Show)

data Statement
  = Assignment {aName :: Identifier, aValue :: Expression}
  | ProcCall {pcName :: Identifier, pcParams :: [Expression]}
  | Compound [Statement]
  | If {iCondition :: Expression, iIfRoute :: Statement, iElseRoute :: Maybe Statement}
  | While {wCondition :: Expression, wBody :: Statement}
  | Repeat {rCondition :: Expression, rBody :: [Statement]}
  deriving (Show)

data Expression
  = Val Value
  | VarRef Identifier
  | FuncCall {fcName :: Identifier, fcParams :: [Expression]}
  | UnOp UnaryOp Expression
  | BinOp {boOp :: BinaryOp, boLeft :: Expression, boRight :: Expression}
  | Paren Expression
  deriving (Show)

data UnaryOp
  = Not
  | UnaryPlus
  | UnaryMinus
  deriving (Show, Eq, Ord)

data BinaryOp
  = Plus
  | Minus
  | Mul
  | Div
  | FullDiv
  | Mod
  | Eql
  | Neql
  | Gt
  | Gte
  | Lt
  | Lte
  | And
  | Or
  | Xor
  deriving (Show, Eq, Ord)

data Identifier = Identifier {idValue :: String}
  deriving (Show)

data DataType
  = DTBoolean
  | DTInteger
  | DTReal
  | DTChar
  | DTString
  deriving (Show, Eq)

data Value
  = Boolean Bool
  | IntNum Int
  | RealNum Double
  | Character Char
  | Str String
  deriving (Show)

printValue :: Value -> String
printValue (Boolean v) = show v
printValue (IntNum v) = show v
printValue (RealNum v) = show v
printValue (Character v) = [v]
printValue (Str v) = v