module Parser (applyParser) where

import Control.Monad
import Text.Parsec.Prim (ParsecT, getParserState)
import Text.ParserCombinators.Parsec
import Lexic

reservedKeywords :: [String]
reservedKeywords = []

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
        ident <- lexemeP identifierP
        _ <- lexemeP (char ';')
        return ident

-- https://www.freepascal.org/docs-html/current/ref/refse115.html#x236-26000016.5
blockP :: Parser Block
blockP = do
  declarations <- many (varDeclP <|> funcDeclP <|> procDeclP)
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
  varName <- lexemeP identifierP
  _ <- lexemeP (char ':')
  typeName <- lexemeP identifierP
  _ <- lexemeP (char ';')
  return (VarDecl [Var {vName = varName, vType = typeName, vValue = Nothing}])

funcDeclP :: Parser Declaration
funcDeclP = do
  _ <- lexeme1P (string "function")
  name <- lexemeP (identifierP)
  formalParams <- parenthesisP formalParamListP
  _ <- lexemeP (char ':')
  returnType <- lexemeP identifierP
  _ <- lexemeP (char ';')
  block <- blockP
  _ <- lexemeP (char ';')
  return (FuncDecl Function {fName = name, fParams = formalParams, fResType = returnType, fBlock = block})

procDeclP :: Parser Declaration
procDeclP = do
  _ <- lexeme1P (string "procedure")
  name <- lexemeP (identifierP)
  formalParams <- parenthesisP formalParamListP
  _ <- lexemeP (char ';')
  block <- blockP
  _ <- lexemeP (char ';')
  return (ProcDecl Procedure {pName = name, pParams = formalParams, pBlock = block})

formalParamListP :: Parser [FormalParam]
formalParamListP = sepBy formalParamP (lexemeP (char ','))
  where
    formalParamP = do
      paramName <- lexemeP identifierP
      _ <- lexemeP (char ':')
      paramType <- lexemeP identifierP
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
statementP = try simpleStatementP <|> structuredStatementP

simpleStatementP :: Parser Statement
simpleStatementP = try assignmentP <|> procCallP

structuredStatementP :: Parser Statement
structuredStatementP = try compoundStatementP <|> conditionalStatementP <|> repetitiveStatementP

conditionalStatementP :: Parser Statement
conditionalStatementP = ifStatementP

repetitiveStatementP :: Parser Statement
repetitiveStatementP = whileStatementP -- forStatementP <|> repeatStatementP <|>

assignmentP :: Parser Statement -- TODO: consider adding ops like +=, -=, etc.
assignmentP = do
  iden <- lexemeP identifierP
  _ <- lexemeP (string ":=")
  expr <- expressionP
  return (Assignment {aName = iden, aValue = expr})

procCallP :: Parser Statement
procCallP = do
  iden <- lexemeP identifierP
  exprs <- parenthesisP paramListP
  return (ProcCall {pcName = iden, pcParams = exprs})

compoundStatementP :: Parser Statement
compoundStatementP = do
  _ <- lexeme1P (string "begin")
  sttm <- endBy statementP (lexemeP (char ';'))
  _ <- lexemeP (string "end")
  return (Compound sttm)

ifStatementP :: Parser Statement
ifStatementP = do
  _ <- lexeme1P (string "if")
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

forStatementP :: Parser Statement
forStatementP = undefined

whileStatementP :: Parser Statement
whileStatementP = do
  _ <- lexeme1P (string "while")
  cond <- expressionP
  _ <- lexeme1P (string "do")
  sttm <- statementP
  return (While { wCondition = cond, wBody = sttm })

repeatStatementP :: Parser Statement
repeatStatementP = undefined

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
-- expressionP = do
--   expr <- simpleExpressionP
--   return (expr)


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
-- simpleExpressionP = do
--   expr <- termP
--   return (expr)

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
-- termP = do
--   _ <- anySpacesP
--   expr <- factorP
--   return (expr)

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
          iden <- lexemeP identifierP
          ch <- optionMaybe (char '(')
          (case ch of
             Just _ -> funcCallP iden
             _ -> return (VarRef iden))
          where
            funcCallP iden = do
              exprs <- paramListP
              _ <- lexemeP (char ')')
              return (FuncCall {fcName = iden, fcParams = exprs})
        unsignedConstantP = do
          v <- valueP
          return (Val v)

paramListP :: Parser [Expression]
paramListP = sepBy expressionP (lexemeP (char ','))

valueP :: Parser Value
valueP = unsignedNumberP <|> boolP
  where unsignedNumberP = do
          number <- lexemeP numberP
          return (IntNum (read number))
        boolP = do
          v <- lexemeP (string "true" <|> string "false")
          return (case v of
            "true" -> Boolean True
            "false" -> Boolean False)

-- https://www.freepascal.org/docs-html/current/ref/refse4.html#x15-140001.4
identifierP :: Parser Identifier
identifierP = do
  bg <- letter <|> underscore
  rest <- many (letter <|> digit <|> underscore)
  return (Identifier (bg:rest))
  where underscore = char '_'


numberP :: Parser String
numberP = many1 digit

parenthesisP :: Parser a -> Parser a
parenthesisP = between (lexemeP (char '(')) (lexemeP (char ')'))

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