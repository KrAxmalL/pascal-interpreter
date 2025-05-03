module Analyzer where

import Lexic
import Data.List
import Control.Applicative

data AnalysisError = IdentifierAlreadyDefinedError String |
                     TypeDoesNotExistError String |
                     VariableDoesNotExistError String |
                     FunctionDoesNotExistError String |
                     ProcedureDoesNotExistError String |
                     ParameterMismatchError String |
                     TypeMismatchError String
                     deriving (Show)

data SymbolTable v = ST {stTable :: [(String, v)]} deriving (Show)

data TypeInfo = TI {tiName :: String} deriving (Show, Eq)
data VarInfo = VI {viName :: String, viType :: TypeInfo} deriving (Show)
data FuncInfo = FI { fiName :: String, fiParams :: [ParamInfo], fiResType :: TypeInfo} deriving (Show)
data ProcInfo = PRI { priName :: String, priParams :: [ParamInfo]} deriving (Show)
data ParamInfo = PAI {paiName :: String, paiType :: TypeInfo} deriving (Show)

data Scope = Scope {
    types :: SymbolTable TypeInfo,
    variables :: SymbolTable VarInfo,
    functions :: SymbolTable FuncInfo,
    procedures :: SymbolTable ProcInfo,
    scopeLevel :: Int,
    parentScope :: Maybe Scope
} deriving (Show)

data Analyzer = A {
    currentScope :: Scope,
    globalScope :: Scope
} deriving (Show)

getByKey :: SymbolTable a -> String -> Maybe a
getByKey (ST l) iden = fmap snd (find (\e -> (fst e) == iden) l)

getType :: Scope -> String -> Either AnalysisError TypeInfo
getType sc tn = case getByKey (types sc) tn of
        Just typeInfo -> Right typeInfo
        Nothing -> case (parentScope sc) of
            Just psc -> getType psc tn
            Nothing -> Left (TypeDoesNotExistError ("Type with name '" ++ tn ++ "' doesn't exist"))

getVar :: Scope -> String -> Either AnalysisError VarInfo
getVar sc vn = case getByKey (variables sc) vn of
        Just varInfo -> Right varInfo
        Nothing -> case (parentScope sc) of
            Just psc -> getVar psc vn
            Nothing -> Left (VariableDoesNotExistError ("Variable with name '" ++ vn ++ "' doesn't exist"))

getFunc :: Scope -> String -> Either AnalysisError FuncInfo
getFunc sc fn = case getByKey (functions sc) fn of
        Just funcInfo -> Right funcInfo
        Nothing -> case (parentScope sc) of
            Just psc -> getFunc psc fn
            Nothing -> Left (FunctionDoesNotExistError ("Function with name '" ++ fn ++ "' doesn't exist"))

getProc :: Scope -> String -> Either AnalysisError ProcInfo
getProc sc pr = case getByKey (procedures sc) pr of
        Just procInfo -> Right procInfo
        Nothing -> case (parentScope sc) of
            Just psc -> getProc psc pr
            Nothing -> Left (ProcedureDoesNotExistError ("Procedure with name '" ++ pr ++ "' doesn't exist"))

createNestedScope :: Scope -> Scope
createNestedScope psc = Scope {
    types = ST {stTable = []},
    variables = ST {stTable = []},
    functions = ST {stTable = []},
    procedures = ST {stTable = []},
    scopeLevel = (scopeLevel psc) + 1,
    parentScope = Just psc
    }

findInScope :: Scope -> String -> Maybe AnalysisError
findInScope Scope {
     types,
     variables,
     functions,
     procedures,
     scopeLevel,
     parentScope
 } iden = fmap (createError ("Type with name '" ++ iden ++ "' already defined")) (getByKey types iden)
            <|> fmap (createError ("Variable with name '" ++ iden ++ "' already defined")) (getByKey variables iden)
            <|> fmap (createError ("Function with name '" ++ iden ++ "' already defined")) (getByKey functions iden)
            <|> fmap (createError ("Procedure with name '" ++ iden ++ "' already defined")) (getByKey procedures iden)
        where createError m _ = IdentifierAlreadyDefinedError m

isGlobalScope :: Scope -> Bool
isGlobalScope sc = case parentScope sc of
    Nothing -> True
    _ -> False

integerTypeInfo :: TypeInfo
integerTypeInfo = TI {tiName = "Integer"}

booleanTypeInfo :: TypeInfo
booleanTypeInfo = TI {tiName = "Boolean"}
startingAnalyzer :: Analyzer
startingAnalyzer = A {
    currentScope = sc,
    globalScope = sc}
    where sc = Scope {
        types = ST {stTable = [("Integer", integerTypeInfo), ("Boolean", booleanTypeInfo)]},
     variables = ST {stTable = []},
     functions = ST {stTable = []},
     procedures = ST {stTable = []}, -- TODO: consider adding writeln and readln
     scopeLevel = 1,
     parentScope = Nothing
    }

applyAnalyzer :: Program -> Either AnalysisError Program
applyAnalyzer p = fmap (const p) (analyzeProgram startingAnalyzer p)

analyzeProgram :: Analyzer -> Program -> Either AnalysisError Analyzer
analyzeProgram a Program {pHeader, pBody} = analyzeBlock (Right a) pBody

analyzeBlock :: Either AnalysisError Analyzer -> Block -> Either AnalysisError Analyzer
analyzeBlock a@(Left _) _ = a
analyzeBlock a@(Right _) Block {bDeclarations, bBody} = do
    a' <- foldl analyzeDeclaration a bDeclarations
    analyzeStatement (Right a') bBody

analyzeDeclaration :: Either AnalysisError Analyzer -> Declaration -> Either AnalysisError Analyzer
analyzeDeclaration a@(Left _) _ = a
analyzeDeclaration a@(Right _) (VarDecl vars) = foldl analyzeVar a vars
    where analyzeVar a v =
            let varName = idValue (vName v)
                varType = idValue (vType v) in case a of
                    Left er -> Left er
                    Right a' -> case (findInScope (currentScope a') varName) of
                        Just er -> Left er
                        Nothing -> case (getType (currentScope a') varType) of
                            Left er -> Left er
                            Right typeInfo -> let updatedScope = (currentScope a') {variables = ST ((varName, VI{viName = varName, viType = typeInfo}) : (stTable (variables (currentScope a'))))}
                                              in Right a' {globalScope = if (isGlobalScope updatedScope) then updatedScope else (globalScope a'), currentScope = updatedScope}
analyzeDeclaration (Right a) (FuncDecl fn) = case (findInScope (currentScope a) fnName) of -- 1. Verify function name is available
                Just er -> Left er
                Nothing -> case (getType (currentScope a) (idValue (fResType fn))) of -- 2. Verify function return type is valid
                    Left er -> Left er
                    Right typeInfo ->  let currScope = currentScope a
                                           newScope = createNestedScope currScope
                                           updatedNewScope = newScope {variables = ST ((fnName, VI {viName = fnName, viType = typeInfo}) : stTable (variables newScope))} -- 2.1 Add variable with function name (used to return values)
                                       in case (analyzeFormalParamList updatedNewScope (fParams fn)) of -- 3. Verify function parameters have unique names and valid types
                                            Left er -> Left er
                                            Right analyzedNewScope -> let updatedParentScope = currScope {functions = ST ((fnName, FI { fiName = fnName, fiParams = map (\p -> PAI {paiName = (idValue (fpName p)), paiType = getRight (getType currScope (idValue (fpType p)))}) (fParams fn) , fiResType = typeInfo}) : (stTable (functions currScope)))}
                                                                          finalNewScope = analyzedNewScope {parentScope = Just updatedParentScope}
                                                                          updatedAnalyzer = Right a {globalScope = if (isGlobalScope updatedParentScope) then updatedParentScope else (globalScope a), currentScope = finalNewScope}
                                                                      in analyzeBlock updatedAnalyzer (fBlock fn)
            where fnName = idValue $ fName fn
analyzeDeclaration (Right a) (ProcDecl pr) = case (findInScope (currentScope a) prName) of -- 1. Verify procedure name is available
                Just er -> Left er
                Nothing -> let currScope = currentScope a
                               newScope = createNestedScope currScope
                           in case (analyzeFormalParamList newScope (pParams pr)) of -- 2. Verify procedure parameters have unique names and valid types
                                Left er -> Left er
                                Right analyzedNewScope -> let updatedParentScope = currScope {procedures = ST ((prName, PRI { priName = prName, priParams = map (\p -> PAI {paiName = (idValue (fpName p)), paiType = getRight (getType currScope (idValue (fpType p)))}) (pParams pr)}) : (stTable (procedures currScope)))}
                                                              finalNewScope = analyzedNewScope {parentScope = Just updatedParentScope}
                                                              updatedAnalyzer = Right a {globalScope = if (isGlobalScope updatedParentScope) then updatedParentScope else (globalScope a), currentScope = finalNewScope}
                                                          in analyzeBlock updatedAnalyzer (pBlock pr)
            where prName = idValue $ pName pr

analyzeFormalParamList :: Scope -> [FormalParam] -> Either AnalysisError Scope
analyzeFormalParamList sc = foldl analyzeFormalParam (Right sc)

analyzeFormalParam :: Either AnalysisError Scope -> FormalParam -> Either AnalysisError Scope
analyzeFormalParam sc p = case sc of
            Left er -> Left er
            Right sc' -> case (findInScope sc' paramName) of
                Just er -> Left er
                Nothing -> case (getType sc' paramType) of
                    Left er -> Left er
                    Right typeInfo -> Right sc' {variables = ST ((paramName, VI {viName = paramName, viType = typeInfo}) : (stTable (variables (sc'))))}
            where paramName = idValue (fpName p)
                  paramType = idValue (fpType p)

analyzeStatement :: Either AnalysisError Analyzer -> Statement -> Either AnalysisError Analyzer
analyzeStatement a@(Left _) _ = a
analyzeStatement ea@(Right a) Assignment {aName, aValue} = case getVar (currentScope a) (idValue aName) of
    Left (VariableDoesNotExistError m) -> Left (VariableDoesNotExistError ("Error during assignment statement! " ++ m))
    Right vi -> case analyzeExpression ea aValue of
        Left (TypeMismatchError m) -> Left (TypeMismatchError ("Wrong expression in assignment statement! " ++ m))
        Right ti -> case expectType aValue (viType vi) ti of
            Left (TypeMismatchError m) -> Left (TypeMismatchError ("Wrong expression type in assignment statement! " ++ m))
            Right _ -> Right a
analyzeStatement (Right a) ProcCall {pcName, pcParams} = do
    pri <- getProc (currentScope a) (idValue pcName)
    analyzeActualParams a (priParams pri) (pcParams)
analyzeStatement a@(Right _) (Compound sttms) = foldl analyzeStatement a sttms
analyzeStatement a@(Right _) If {iCondition, iIfRoute, iElseRoute} = case (analyzeExpression a iCondition) of
    Left (TypeMismatchError m) -> Left (TypeMismatchError ("Error in expression in 'if' statement! " ++ m))
    Right ti -> case expectType iCondition booleanTypeInfo ti of
        Left (TypeMismatchError m) -> Left (TypeMismatchError ("Wrong expression type in 'if' statement! " ++ m))
        Right _ -> analyzeStatement a wBody
analyzeStatement a@(Right _) While {wCondition, wBody} = case (analyzeExpression a wCondition) of
    Left (TypeMismatchError m) -> Left (TypeMismatchError ("Error in expression in 'while' statement! " ++ m))
    Right ti -> case expectType wCondition booleanTypeInfo ti of
        Left (TypeMismatchError m) -> Left (TypeMismatchError ("Wrong expression type in 'while' statement! " ++ m))
        Right _ -> analyzeStatement a wBody

analyzeExpression :: Either AnalysisError Analyzer -> Expression -> Either AnalysisError TypeInfo
analyzeExpression a@(Left er) _ = Left er
analyzeExpression a@(Right _) (Val val) = case val of
    IntNum _ -> Right integerTypeInfo
    Boolean _ -> Right booleanTypeInfo
    _ -> error "unreachable"
analyzeExpression (Right a) (VarRef iden) = fmap viType (getVar (currentScope a) (idValue iden))
analyzeExpression (Right a) (FuncCall {fcName, fcParams}) = do
    fi <- getFunc (currentScope a) (idValue fcName)
    _ <- analyzeActualParams a (fiParams fi) (fcParams)
    return (fiResType fi)
analyzeExpression a@(Right _) (UnOp unOp expr) = case (unOp, analyzeExpression a expr) of
    (_, Left er) -> Left er
    (Not, Right ti) -> expectType expr booleanTypeInfo ti
    (UnaryPlus, Right ti) -> expectType expr integerTypeInfo ti
    (UnaryMinus, Right ti) -> expectType expr integerTypeInfo ti
analyzeExpression a@(Right _) (BinOp {boOp, boLeft, boRight}) = case (boOp, analyzeExpression a boLeft, analyzeExpression a boRight) of
    (_, Left (TypeMismatchError lm), Left (TypeMismatchError rm)) -> Left (TypeMismatchError ("Left operand error:" ++ lm ++ ". Right operand error: " ++ rm))
    (_, Left er, _) -> Left er
    (_, _, Left er) -> Left er
    (op, Right lti, Right rti)
        | elem op [Plus, Minus, Mul, Div, FullDiv, Mod] -> expectBinOpTypes integerTypeInfo boLeft [integerTypeInfo] lti boRight [integerTypeInfo] rti True
        | elem op [Eql, Neql] -> expectBinOpTypes booleanTypeInfo boLeft [integerTypeInfo, booleanTypeInfo] lti boRight [integerTypeInfo, booleanTypeInfo] rti True
        | elem op [Gt, Gte, Lt, Lte] -> expectBinOpTypes booleanTypeInfo boLeft [integerTypeInfo] lti boRight [integerTypeInfo] rti True
        | elem op [And, Or, Xor] -> expectBinOpTypes booleanTypeInfo boLeft [booleanTypeInfo] lti boRight [booleanTypeInfo] rti True
analyzeExpression a@(Right _) (Paren expr) = analyzeExpression a expr

expectBinOpTypes :: TypeInfo -> Expression -> [TypeInfo] -> TypeInfo -> Expression -> [TypeInfo] -> TypeInfo -> Bool -> Either AnalysisError TypeInfo
expectBinOpTypes retTi exprl exl lti exprr exr rti shouldBeEqual = case (expectTypes exprl exl lti, expectTypes exprr exr rti) of
    (Left (TypeMismatchError lm), Left (TypeMismatchError rm)) -> Left (TypeMismatchError ("Left operand error: " ++ lm ++ ". Right operand error: " ++ rm))
    (Left er, _) -> Left er
    (_, Left er) -> Left er
    (Right flti, Right frti) ->
        if shouldBeEqual
        then (if flti == frti then Right retTi else Left (TypeMismatchError ("Left operand type is not equal to right operand type! Left operand type: " ++ (tiName flti) ++ ". Right operand type: " ++ (tiName frti))))
        else Right retTi

expectType :: Expression -> TypeInfo -> TypeInfo -> Either AnalysisError TypeInfo
expectType expr ex = expectTypes expr [ex]

expectTypes :: Expression -> [TypeInfo] -> TypeInfo -> Either AnalysisError TypeInfo
expectTypes expr exl ac = if elem ac exl
    then Right ac
    else Left (TypeMismatchError ("Expression: " ++ (show expr) ++ " has wrong type! Expected one of these types: " ++ (show (map tiName exl)) ++ ". Actual type: " ++ (tiName ac)))

analyzeActualParams :: Analyzer -> [ParamInfo] -> [Expression] -> Either AnalysisError Analyzer
analyzeActualParams a fps aps = if fpsl /= apsl
    then Left (ParameterMismatchError ("Amounts of formal and actual parameters are different! FP amount: " ++ (show fpsl) ++ ", AP amount: " ++ (show apsl)))
    else foldl analyzeParamType (Right a) (zip fps aps)
    where fpsl = length fps
          apsl = length aps
          analyzeParamType a' p = case (a', p) of
            (Left er, _) -> Left er
            (a''@(Right _), (fp, ap)) -> case analyzeExpression a'' ap of
                Left er -> Left er
                Right ti -> if (ti == (paiType fp))
                    then a''
                    else Left (ParameterMismatchError ("Formal parameter '" ++ (paiName fp) ++ "' has wrong type! Expected: " ++ (tiName (paiType fp)) ++ ". Actual: " ++ (tiName ti)))

testVarDecl :: Declaration
testVarDecl = VarDecl
    [
        Var { vName = Identifier {idValue = "a"}, vType = Identifier {idValue = "Integer"}, vValue = Nothing },
        Var { vName = Identifier {idValue = "b"}, vType = Identifier {idValue = "Boolean"}, vValue = Nothing }
    ]

testFuncDecl :: Declaration
testFuncDecl = FuncDecl Function {
        fName = Identifier {idValue = "fn"},
        fParams = [FormalParam {fpName = Identifier {idValue = "a"}, fpType = Identifier {idValue = "Boolean"}}],
        fResType = Identifier {idValue = "Integer"},
        fBlock = Block {
            bDeclarations = [],
            bBody = Compound []
        }
    }

testProcDecl :: Declaration
testProcDecl = ProcDecl Procedure {
        pName = Identifier {idValue = "pr"},
        pParams = [FormalParam {fpName = Identifier {idValue = "a"}, fpType = Identifier {idValue = "Integer"}}],
        pBlock = Block {
            bDeclarations = [],
            bBody = Compound []
        }
    }

getRight :: Either a b -> b
getRight (Right v) = v
getRight _ = error "Either doesn't have Right value"




testIntNumExpr :: Expression
testIntNumExpr = Val (IntNum 42)

testBooleanExpr :: Expression
testBooleanExpr = Val (Boolean True)

testVarRefExpr :: Expression
testVarRefExpr = VarRef (Identifier {idValue = "c"})

testFuncCallExpr :: Expression
testFuncCallExpr = FuncCall {fcName = Identifier {idValue = "fn"}, fcParams =[
    Val (Boolean True)
]}

testBinOpNumeric :: Expression
testBinOpNumeric = BinOp {boOp = Plus, boLeft = Val (IntNum 42), boRight = Val (IntNum 42)}

testBinOpEquality :: Expression
testBinOpEquality = BinOp {boOp = Eql, boLeft = Val (Boolean True), boRight = Val (Boolean True)}

testBinOpComparison :: Expression
testBinOpComparison = BinOp {boOp = Gt, boLeft = Val (IntNum 42), boRight = Val (IntNum 42)}

testBinOpLogical :: Expression
testBinOpLogical = BinOp {boOp = And, boLeft = Val (Boolean True), boRight = Val (Boolean True)}

testSttmAssignment :: Statement
testSttmAssignment = Assignment {aName = Identifier {idValue = "a"}, aValue = Val (IntNum 42) }

testSttmProcCall :: Statement
testSttmProcCall = ProcCall {pcName = Identifier {idValue = "pr"}, pcParams =[
    Val (IntNum 42)
]}

testSttmWhile :: Statement
testSttmWhile = While { wCondition = Val (IntNum 42), wBody = Compound []}