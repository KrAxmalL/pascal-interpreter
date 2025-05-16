module Parser (applyParser) where

import Control.Monad
import Text.Parsec.Prim (ParsecT, getParserState)
import Text.ParserCombinators.Parsec
import qualified Data.Char as Ch
import Lexic

-- TODO: add more reserved keywords
reservedKeywords :: [String]
reservedKeywords = ["program", "var", "function", "procedure", "begin", "end", "if", "then", "else", "while", "do", "div", "mod", "not", "and", "or", "xor", "true", "false"]

applyParser :: String -> String -> Either ParseError Program
applyParser = parse programP

-- https://www.freepascal.org/docs-html/current/ref/refse111.html#x232-25600016.1
programP :: Parser Program
programP = do
  _ <- anySpacesP
  name <- headerP
  block <- blockP
  _ <- lexemeP (char '.')
  pos <- sourcePos
  return (Program {pHeader = name, pBody = block, pPos = pos})
  where
    headerP = do
        _ <- lexemeP (string "program")
        ident <- lexemeP allowedIdentifierP
        _ <- lexemeP semicolonP
        return ident

-- https://www.freepascal.org/docs-html/current/ref/refse115.html#x236-26000016.5
blockP :: Parser Block
blockP = do
  declarations <- many (varDeclP <|> funcDeclP <|> procDeclP)
  _ <- lexemeP (string "begin")
  sttm <- compoundStatementP
  return (Block { bDeclarations = declarations,
     bBody = sttm})

-- https://www.freepascal.org/docs-html/current/ref/refse22.html
{- TODO:
    1. Add list of vars like var a,b,c : integer;
    2. Add inlined variable initialization like var a : integer = 1;
-}
varDeclP :: Parser Declaration
varDeclP = do
  _ <- lexeme1P (string "var")
  varName <- lexemeP allowedIdentifierP
  _ <- lexemeP colonP
  varType <- dataTypeP
  _ <- lexemeP semicolonP
  return (VarDecl [Var {vName = varName, vType = varType, vValue = Nothing}])

funcDeclP :: Parser Declaration
funcDeclP = do
  _ <- lexeme1P (string "function")
  name <- lexemeP allowedIdentifierP
  formalParams <- parenthesisP formalParamListP
  _ <- lexemeP colonP
  returnType <- dataTypeP
  _ <- lexemeP semicolonP
  block <- blockP
  _ <- lexemeP semicolonP
  return (FuncDecl Function {fName = name, fParams = formalParams, fResType = returnType, fBlock = block})

procDeclP :: Parser Declaration
procDeclP = do
  _ <- lexeme1P (string "procedure")
  name <- lexemeP allowedIdentifierP
  formalParams <- parenthesisP formalParamListP
  _ <- lexemeP semicolonP
  block <- blockP
  _ <- lexemeP semicolonP
  return (ProcDecl Procedure {pName = name, pParams = formalParams, pBlock = block})

formalParamListP :: Parser [FormalParam]
formalParamListP = sepBy formalParamP (lexemeP semicolonP)
  where
    formalParamP = do
      paramName <- lexemeP allowedIdentifierP
      _ <- lexemeP colonP
      paramType <- lexemeP dataTypeP
      return (FormalParam {fpName = paramName, fpType = paramType} )

integerP :: Parser Integer
integerP = do
  sign <- optionMaybe (char '+' <|> char '-')
  n <- many1 digit
  return (read (
    case sign of
      Just s -> if s == '-' then s:n else n -- "read" can't parse numbers like "+23"
      Nothing -> n))

statementP :: Parser Statement
statementP = do 
  iden <- lexemeP identifierP
  case idValue iden of
    "if" -> ifStatementP
    "while" -> whileStatementP
    "repeat" -> repeatStatementP
    "begin" -> compoundStatementP
    _ -> assignmentOrProcCallP iden
    where 
      ifStatementP = do
        cond <- expressionP
        _ <- lexeme1P (string "then")
        ifSttm <- statementP
        hasElse <- optionMaybe (lexeme1P (string "else"))
        (case hasElse of
          Just _ -> (do
            elseSttm <- statementP
            return (If { iCondition = cond,
            iIfRoute = ifSttm,
            iElseRoute = Just elseSttm}))
          Nothing -> return ( If { iCondition = cond,
          iIfRoute = ifSttm,
          iElseRoute = Nothing}))
      whileStatementP = do
        cond <- expressionP
        _ <- lexeme1P (string "do")
        sttm <- statementP
        return (While { wCondition = cond, wBody = sttm })
      repeatStatementP = do
        sttms <- statementListP
        _ <- lexemeP (string "until")
        cond <- expressionP
        return (Repeat {rCondition = cond, rBody = sttms})
      assignmentOrProcCallP iden = do
        ch <- optionMaybe openParen
        case ch of
          Just _ -> procCallP
          Nothing -> assignmentP
          where
            procCallP = do
              exprs <- paramListP
              _ <- lexemeP closeParen
              return (ProcCall {pcName = iden, pcParams = exprs})
            assignmentP = do
              _ <- lexemeP (string ":=")
              expr <- expressionP
              return (Assignment {aName = iden, aValue = expr})

compoundStatementP :: Parser Statement
compoundStatementP = do
  sttms <- statementListP
  _ <- lexemeP (string "end")
  return (Compound sttms)

statementListP :: Parser [Statement]
statementListP = endBy (try statementP) (lexemeP semicolonP)

-- https://www.freepascal.org/docs-html/current/ref/refse81.html#x143-16700012.1
expressionP :: Parser Expression
expressionP = chainl1 simpleExpressionP binOpC
  where
    binOpC = do
      op <- lexemeP (try (string ">=") <|>
           try (string "<=") <|>
           try (string "<>") <|>
           string ">"  <|>
           string "<"  <|>
           string "=")
      return (binOpConstructor $ binOp op)

simpleExpressionP :: Parser Expression
simpleExpressionP = chainl1 termP binOpC
  where
    binOpC = do
      op <- lexemeP (
           string "+"  <|>
           string "-"  <|>
           try (string "or") <|>
           try (string "xor"))
      return (binOpConstructor $ binOp op)

termP :: Parser Expression
termP = chainl1 factorP binOpC
  where
    binOpC = do
      op <- lexemeP (
           string "*"  <|>
           string "/"  <|>
           try (string "div") <|>
           try (string "mod") <|>
           try (string "and"))
      return (binOpConstructor $ binOp op)

binOpConstructor :: BinaryOp -> Expression -> Expression -> Expression
binOpConstructor op l r = BinOp {boOp = op, boLeft = l, boRight = r}

binOp :: String -> BinaryOp
binOp s = case s of
  "+" -> Plus
  "-" -> Minus
  "*" -> Mul
  "/" -> Div
  "div" -> FullDiv
  "mod" -> Mod
  "=" -> Eql
  "<>" -> Neql
  ">" -> Gt
  ">=" -> Gte
  "<" -> Lt
  "<=" -> Lte
  "and" -> And
  "or" -> Or
  "xor" -> Xor
  _  -> error "unreachable"

factorP :: Parser Expression
factorP = do
  _ <- anySpacesP
  expr <- parenExpressionP <|>
          notFactorP <|>
          signFactorP <|>
          unsignedConstantP <|>
          varRefOrFuncCallP
  return (expr)
  where parenExpressionP = do
          expr <- parenthesisP expressionP
          return (Paren expr)
        notFactorP = do
          _ <- try (lexeme1P (string "not"))
          expr <- lexemeP factorP
          return (UnOp Not expr)
        signFactorP = do
            sign <- lexemeP (oneOf "+-")
            expr <- lexemeP factorP
            return (UnOp (if sign == '+' then UnaryPlus else UnaryMinus) expr)
        varRefOrFuncCallP = do
          iden <- lexemeP allowedIdentifierP
          ch <- optionMaybe openParen
          (case ch of
             Just _ -> funcCallP iden
             Nothing -> return (VarRef iden))
          where
            funcCallP iden = do
              exprs <- paramListP
              _ <- lexemeP closeParen
              return (FuncCall {fcName = iden, fcParams = exprs})
        unsignedConstantP = do
          v <- valueP
          return (Val v)

paramListP :: Parser [Expression]
paramListP = sepBy expressionP (lexemeP (char ','))

valueP :: Parser Value
valueP = unsignedNumberP <|> boolP <|> stringP
  where unsignedNumberP = do
          integralPart <- numberP
          dot <- optionMaybe (char '.')
          case dot of
            Nothing -> do 
              _ <- anySpacesP
              return (IntNum (read integralPart))
            Just _ -> do
              fractionalPart <- lexemeP numberP
              return (RealNum (read (integralPart ++ "." ++ fractionalPart)))
        boolP = do
          v <- lexemeP (try (string "true") <|> try (string "false"))
          return (case v of
            "true" -> Boolean True
            "false" -> Boolean False)
        stringP = do
          fst <- singleQuoteP <|> hashP
          str <- case fst of
              '\'' -> do
                chs <- many (try ((noneOf "'") <|> (char '\'' >> char '\'')))
                _ <- lexemeP (char '\'')
                return chs
              '#' -> do
                fst <- numberP
                rest <- many (hashP >> numberP)
                _ <- anySpacesP
                return (map (toEnum . read) (fst:rest))
          return (if length str == 1 then (Character (head str)) else (Str str))

dataTypeP :: Parser DataType
dataTypeP = do
          v <- lexemeP (try (string "Integer") <|> try (string "Real") <|> try (string "Boolean") <|> try (string "Char") <|> try (string "String"))
          return (case v of
            "Integer" -> DTInteger
            "Real" -> DTReal
            "Boolean" -> DTBoolean
            "Char" -> DTChar
            "String" -> DTString)

-- https://www.freepascal.org/docs-html/current/ref/refse4.html#x15-140001.4
identifierP :: Parser Identifier
identifierP = do
  bg <- letter <|> underscoreP
  rest <- many (letter <|> digit <|> underscoreP)
  return (Identifier {idValue = bg:rest})
  where underscoreP = char '_'

allowedIdentifierP :: Parser Identifier
allowedIdentifierP = do
  iden@(Identifier {idValue}) <- identifierP
  if (elem idValue reservedKeywords)
  then unexpected idValue
  else return (iden)

numberP :: Parser String
numberP = many1 digit

hashP :: Parser Char
hashP = char '#'

singleQuoteP :: Parser Char
singleQuoteP = char '\''

colonP :: Parser Char
colonP = char ':'

semicolonP :: Parser Char
semicolonP = char ';'

openParen :: Parser Char
openParen = char '('

closeParen :: Parser Char
closeParen = char ')'

parenthesisP :: Parser a -> Parser a
parenthesisP = between (lexemeP openParen) (lexemeP closeParen)

lexeme1P :: Parser a -> Parser a
lexeme1P p = do
  x <- p
  _ <- anySpaces1P
  return x

lexemeP :: Parser a -> Parser a
lexemeP p = do
  x <- p
  _ <- anySpacesP
  return x

anySpaces1P :: Parser ()
anySpaces1P = skipMany1 anySpace

anySpacesP :: Parser ()
anySpacesP = skipMany anySpace

anySpace :: Parser Char
anySpace = space <|> newline <|> tab

tParse :: Parser a -> String -> Either ParseError a
tParse p = parse p "source"

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `liftM` getParserState