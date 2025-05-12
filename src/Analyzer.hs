module Analyzer(applyAnalyzer, printAnalysisError) where

import Lexic
import Data.List
import qualified Data.Map.Strict as Map
import Control.Applicative
import Data.Maybe

data AnalysisError = AnalysisError AnalysisErrorType String (Maybe AnalysisError) deriving (Show)
data AnalysisErrorType = IdentifierAlreadyDefinedError |
                     TypeDoesNotExistError |
                     VariableDoesNotExistError |
                     FunctionDoesNotExistError |
                     ProcedureDoesNotExistError |
                     ActualParameterError |
                     TypeMismatchError |
                     FormalParameterDeclarationError |
                     VariableDeclarationError |
                     FunctionDeclarationError |
                     ProcedureDeclarationError |
                     AssignmentError |
                     ProcedureCallError |
                     IfStatementError |
                     WhileStatementError |
                     VariableReferenceError |
                     FunctionCallError |
                     UnaryOperatorError |
                     BinaryOperatorError
                     deriving (Show)

printAnalysisError :: AnalysisError -> String
printAnalysisError (AnalysisError tp message source) = initialMessage ++ sourceMessage ++ "]"
    where initialMessage = "AnalysisError-[type = " ++ (show tp) ++ ", message = " ++ message
          sourceMessage = case source of
            Nothing -> ""
            Just se -> ", source = " ++ (printAnalysisError se)

data Analyzer = A {
    currentScope :: Scope,
    globalScope :: Scope
} deriving (Show)

data Scope = Scope {
    types :: Map.Map String TypeInfo,
    variables :: Map.Map String VarInfo,
    functions :: Map.Map String FuncInfo,
    procedures :: Map.Map String ProcInfo,
    scopeLevel :: Int,
    parentScope :: Maybe Scope
} deriving (Show)

data TypeInfo = TI {tiName :: String} deriving (Show, Eq)
data VarInfo = VI {viName :: String, viType :: TypeInfo} deriving (Show)
data FuncInfo = FI { fiName :: String, fiParams :: [ParamInfo], fiResType :: TypeInfo} deriving (Show)
data ProcInfo = PRI { priName :: String, priParams :: [ParamInfo]} deriving (Show)
data ParamInfo = PAI {paiName :: String, paiType :: TypeInfo} deriving (Show)

getType :: Scope -> String -> Either AnalysisError TypeInfo
getType sc tn = case Map.lookup tn (types sc) of
        Just typeInfo -> Right typeInfo
        Nothing -> case (parentScope sc) of
            Just psc -> getType psc tn
            Nothing -> Left (AnalysisError TypeDoesNotExistError ("Type with name '" ++ tn ++ "' doesn't exist") Nothing)

getVar :: Scope -> String -> Either AnalysisError VarInfo
getVar sc vn = case Map.lookup vn (variables sc) of
        Just varInfo -> Right varInfo
        Nothing -> case (parentScope sc) of
            Just psc -> getVar psc vn
            Nothing -> Left (AnalysisError VariableDoesNotExistError ("Variable with name '" ++ vn ++ "' doesn't exist") Nothing)

getFunc :: Scope -> String -> Either AnalysisError FuncInfo
getFunc sc fn = case Map.lookup fn (functions sc) of
        Just funcInfo -> Right funcInfo
        Nothing -> case (parentScope sc) of
            Just psc -> getFunc psc fn
            Nothing -> Left (AnalysisError FunctionDoesNotExistError ("Function with name '" ++ fn ++ "' doesn't exist") Nothing)

getProc :: Scope -> String -> Either AnalysisError ProcInfo
getProc sc pr = case Map.lookup pr (procedures sc) of
        Just procInfo -> Right procInfo
        Nothing -> case (parentScope sc) of
            Just psc -> getProc psc pr
            Nothing -> Left (AnalysisError ProcedureDoesNotExistError ("Procedure with name '" ++ pr ++ "' doesn't exist") Nothing)

createNestedScope :: Scope -> Scope
createNestedScope psc = Scope {
    types = Map.empty,
    variables = Map.empty,
    functions = Map.empty,
    procedures = Map.empty,
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
 } iden = fmap (createError ("Type with name '" ++ iden ++ "' already defined")) (Map.lookup iden types)
            <|> fmap (createError ("Variable with name '" ++ iden ++ "' already defined")) (Map.lookup iden variables)
            <|> fmap (createError ("Function with name '" ++ iden ++ "' already defined")) (Map.lookup iden functions)
            <|> fmap (createError ("Procedure with name '" ++ iden ++ "' already defined")) (Map.lookup iden procedures)
        where createError m _ = AnalysisError IdentifierAlreadyDefinedError m Nothing

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
        types = Map.fromList [("Integer", integerTypeInfo), ("Boolean", booleanTypeInfo)],
     variables = Map.empty,
     functions = Map.empty,
     procedures = Map.fromList [("writeln", PRI { priName = "writeln", priParams = []})], -- TODO: consider adding readln
     scopeLevel = 1,
     parentScope = Nothing
    }

applyAnalyzer :: Program -> Either AnalysisError Analyzer
applyAnalyzer p = analyzeProgram startingAnalyzer p

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
                        Just er -> Left (AnalysisError VariableDeclarationError ("Error while declaring variable '" ++ varName ++ "'!") (Just er))
                        Nothing -> case (getType (currentScope a') varType) of
                            Left er -> Left (AnalysisError VariableDeclarationError ("Error while declaring variable '" ++ varName ++ "'!") (Just er))
                            Right typeInfo -> let updatedScope = (currentScope a') {variables = Map.insert varName (VI{viName = varName, viType = typeInfo}) (variables (currentScope a'))}
                                              in Right a' {globalScope = if (isGlobalScope updatedScope) then updatedScope else (globalScope a'), currentScope = updatedScope}
analyzeDeclaration (Right a) (FuncDecl fn) = case (findInScope (currentScope a) fnName) of -- 1. Verify function name is available
                Just er -> Left (AnalysisError FunctionDeclarationError ("Error while declaring function '" ++ fnName ++ "'!") (Just er))
                Nothing -> case (getType (currentScope a) (idValue (fResType fn))) of -- 2. Verify function return type is valid
                    Left er -> Left (AnalysisError FunctionDeclarationError ("Error while declaring function '" ++ fnName ++ "'!") (Just er))
                    Right typeInfo ->  let currScope = currentScope a
                                           newScope = createNestedScope currScope
                                           updatedNewScope = newScope {variables = Map.insert fnName (VI {viName = fnName, viType = typeInfo}) (variables newScope)} -- 2.1 Add variable with function name (used to return values)
                                       in case (analyzeFormalParamList updatedNewScope (fParams fn)) of -- 3. Verify function parameters have unique names and valid types
                                            Left er -> Left (AnalysisError FunctionDeclarationError ("Error while declaring function '" ++ fnName ++ "'!") (Just er))
                                            Right analyzedNewScope -> let updatedParentScope = currScope {functions = Map.insert fnName (FI { fiName = fnName, fiParams = map (\p -> PAI {paiName = (idValue (fpName p)), paiType = getRight (getType currScope (idValue (fpType p)))}) (fParams fn), fiResType = typeInfo}) (functions currScope)}
                                                                          finalNewScope = analyzedNewScope {parentScope = Just updatedParentScope}
                                                                          updatedAnalyzer = Right a {globalScope = if (isGlobalScope updatedParentScope) then updatedParentScope else (globalScope a), currentScope = finalNewScope}
                                                                      in case analyzeBlock updatedAnalyzer (fBlock fn) of
                                                                        Left er -> Left (AnalysisError FunctionDeclarationError ("Error while declaring function '" ++ fnName ++ "'!") (Just er))
                                                                        Right a' -> Right a' {currentScope = fromJust (parentScope (currentScope a'))}
            where fnName = idValue $ fName fn
analyzeDeclaration (Right a) (ProcDecl pr) = case (findInScope (currentScope a) prName) of -- 1. Verify procedure name is available
                Just er -> Left (AnalysisError ProcedureDeclarationError ("Error while declaring procedure '" ++ prName ++ "'!") (Just er))
                Nothing -> let currScope = currentScope a
                               newScope = createNestedScope currScope
                           in case (analyzeFormalParamList newScope (pParams pr)) of -- 2. Verify procedure parameters have unique names and valid types
                                Left er -> Left (AnalysisError ProcedureDeclarationError ("Error while declaring procedure '" ++ prName ++ "'!") (Just er))
                                Right analyzedNewScope -> let updatedParentScope = currScope {procedures = Map.insert prName (PRI { priName = prName, priParams = map (\p -> PAI {paiName = (idValue (fpName p)), paiType = getRight (getType currScope (idValue (fpType p)))}) (pParams pr)}) (procedures currScope)}
                                                              finalNewScope = analyzedNewScope {parentScope = Just updatedParentScope}
                                                              updatedAnalyzer = Right a {globalScope = if (isGlobalScope updatedParentScope) then updatedParentScope else (globalScope a), currentScope = finalNewScope}
                                                          in case analyzeBlock updatedAnalyzer (pBlock pr) of
                                                            Left er -> Left (AnalysisError ProcedureDeclarationError ("Error while declaring procedure '" ++ prName ++ "'!") (Just er))
                                                            Right a' -> Right a' {currentScope = fromJust (parentScope (currentScope a'))}
            where prName = idValue $ pName pr

analyzeFormalParamList :: Scope -> [FormalParam] -> Either AnalysisError Scope
analyzeFormalParamList sc = foldl analyzeFormalParam (Right sc)

analyzeFormalParam :: Either AnalysisError Scope -> FormalParam -> Either AnalysisError Scope
analyzeFormalParam sc p = case sc of
            Left er -> Left er
            Right sc' -> case (findInScope sc' paramName) of
                Just er -> Left (AnalysisError FormalParameterDeclarationError ("Error while declaring formal parameter '" ++ paramName ++ "'!") (Just er))
                Nothing -> case (getType sc' paramType) of
                    Left er -> Left (AnalysisError FormalParameterDeclarationError ("Error while declaring formal parameter '" ++ paramName ++ "'!") (Just er))
                    Right typeInfo -> Right sc' {variables = Map.insert paramName (VI {viName = paramName, viType = typeInfo}) (variables sc')}
            where paramName = idValue (fpName p)
                  paramType = idValue (fpType p)

analyzeStatement :: Either AnalysisError Analyzer -> Statement -> Either AnalysisError Analyzer
analyzeStatement a@(Left _) _ = a
analyzeStatement ea@(Right a) Assignment {aName, aValue} = case getVar (currentScope a) (idValue aName) of
    Left er -> Left (AnalysisError AssignmentError "Error during assignment statement!" (Just er))
    Right vi -> case analyzeExpression ea aValue of
        Left er -> Left (AnalysisError AssignmentError "Wrong expression in assignment statement!" (Just er))
        Right ti -> case expectType aValue (viType vi) ti of
            Left er -> Left (AnalysisError AssignmentError "Wrong expression type in assignment statement!" (Just er))
            Right _ -> Right a
analyzeStatement (Right a) ProcCall {pcName, pcParams} = case getProc (currentScope a) procName of
    Left er -> Left (AnalysisError ProcedureCallError ("Error when calling procedure '" ++ procName ++ "'!") (Just er))
    Right pri -> 
        let isWriteln = (idValue pcName) == "writeln"
            procParams = if isWriteln then [PAI {paiName = "v1", paiType = TI {tiName = ""}}] else priParams pri
            ignoreParamTypes = isWriteln
        in case analyzeActualParams a procParams pcParams ignoreParamTypes of
            Left er -> Left (AnalysisError ProcedureCallError ("Error when calling procedure '" ++ procName ++ "'!") (Just er))
            Right a' -> Right a'
    where procName = idValue pcName
analyzeStatement a@(Right _) (Compound sttms) = foldl analyzeStatement a sttms
analyzeStatement a@(Right _) If {iCondition, iIfRoute, iElseRoute} = case (analyzeExpression a iCondition) of
    Left er -> Left (AnalysisError IfStatementError "Error in conditional expression in 'if' statement!" (Just er))
    Right ti -> case expectType iCondition booleanTypeInfo ti of
        Left er -> Left (AnalysisError IfStatementError "Wrong conditional expression type in 'if' statement!" (Just er))
        Right _ -> case analyzeStatement a iIfRoute of
            Left er -> Left (AnalysisError IfStatementError "Error in 'if' statement!" (Just er))
            ea@(Right _) -> case iElseRoute of
                Nothing -> ea
                Just elseSttm -> case analyzeStatement ea elseSttm of
                    Left er -> Left (AnalysisError IfStatementError "Error in 'if' statement!" (Just er))
                    ea'@(Right _) -> ea'
analyzeStatement a@(Right _) While {wCondition, wBody} = case (analyzeExpression a wCondition) of
    Left er -> Left (AnalysisError WhileStatementError "Error in conditional expression in 'while' statement! "  (Just er))
    Right ti -> case expectType wCondition booleanTypeInfo ti of
        Left er -> Left (AnalysisError WhileStatementError "Wrong conditional expression type in 'while' statement!"  (Just er))
        Right _ -> case analyzeStatement a wBody of
            Left er -> Left (AnalysisError WhileStatementError "Error in 'while' statement!"  (Just er))
            ea@(Right _) -> ea

analyzeExpression :: Either AnalysisError Analyzer -> Expression -> Either AnalysisError TypeInfo
analyzeExpression a@(Left er) _ = Left er
analyzeExpression a@(Right _) (Val val) = case val of
    IntNum _ -> Right integerTypeInfo
    Boolean _ -> Right booleanTypeInfo
analyzeExpression (Right a) (VarRef iden) = case getVar (currentScope a) (idValue iden) of
    Left er -> Left (AnalysisError VariableReferenceError ("Error when referencing variable '" ++ (idValue iden) ++ "'!") (Just er))
    Right vi -> Right (viType vi)
analyzeExpression (Right a) (FuncCall {fcName, fcParams}) = case getFunc (currentScope a) funcName of
    Left er -> Left (AnalysisError FunctionCallError ("Error when calling function '" ++ funcName ++ "'!") (Just er))
    Right fi -> case analyzeActualParams a (fiParams fi) fcParams False of
        Left er -> Left (AnalysisError FunctionCallError ("Error when calling function '" ++ funcName ++ "'!") (Just er))
        Right _ -> Right (fiResType fi)
    where funcName = idValue fcName
analyzeExpression a@(Right _) (UnOp unOp expr) = case (unOp, analyzeExpression a expr) of
    (op, Left er) -> Left (AnalysisError UnaryOperatorError ("Error in expression when using unary operator '" ++ (show op) ++ "'!") (Just er))
    (op, Right ti) -> case getType of
        Left er -> Left (buildError er)
        Right ti -> Right ti
        where buildError er' = AnalysisError UnaryOperatorError ("Error when using unary operator '" ++ (show op) ++ "'!") (Just er')
              getType
                | (op == Not) = expectType expr booleanTypeInfo ti
                | (elem op [UnaryPlus, UnaryMinus]) = expectType expr integerTypeInfo ti
                | otherwise = error "unreachable"
analyzeExpression a@(Right _) (BinOp {boOp, boLeft, boRight}) = case (boOp, analyzeExpression a boLeft, analyzeExpression a boRight) of
    (_, Left (AnalysisError TypeMismatchError lm _), Left (AnalysisError TypeMismatchError rm _)) -> Left (AnalysisError TypeMismatchError ("Left operand error:" ++ lm ++ ". Right operand error: " ++ rm) Nothing)
    (_, Left er, _) -> Left er
    (_, _, Left er) -> Left er
    (op, Right lti, Right rti)  -> case getType of
        Left er -> Left (buildError er)
        Right ti -> Right ti
        where buildError er' = AnalysisError BinaryOperatorError ("Error when using binary operator '" ++ (show op) ++ "'!") (Just er')
              getType
                | elem op [Plus, Minus, Mul, Div, FullDiv, Mod] = expectBinOpTypes integerTypeInfo boLeft [integerTypeInfo] lti boRight [integerTypeInfo] rti True
                | elem op [Eql, Neql] = expectBinOpTypes booleanTypeInfo boLeft [integerTypeInfo, booleanTypeInfo] lti boRight [integerTypeInfo, booleanTypeInfo] rti True
                | elem op [Gt, Gte, Lt, Lte] = expectBinOpTypes booleanTypeInfo boLeft [integerTypeInfo] lti boRight [integerTypeInfo] rti True
                | elem op [And, Or, Xor] = expectBinOpTypes booleanTypeInfo boLeft [booleanTypeInfo] lti boRight [booleanTypeInfo] rti True
analyzeExpression a@(Right _) (Paren expr) = analyzeExpression a expr

-- TODO: finish with improving error handling for binary operators
expectBinOpTypes :: TypeInfo -> Expression -> [TypeInfo] -> TypeInfo -> Expression -> [TypeInfo] -> TypeInfo -> Bool -> Either AnalysisError TypeInfo
expectBinOpTypes retTi exprl exl lti exprr exr rti shouldBeEqual = case (expectTypes exprl exl lti, expectTypes exprr exr rti) of
    (Left (AnalysisError TypeMismatchError lm _), Left (AnalysisError TypeMismatchError rm _)) -> Left (AnalysisError TypeMismatchError ("Left operand error: " ++ lm ++ ". Right operand error: " ++ rm) Nothing)
    (Left er, _) -> Left er
    (_, Left er) -> Left er
    (Right flti, Right frti) ->
        if shouldBeEqual
        then (if flti == frti then Right retTi else Left (AnalysisError TypeMismatchError ("Left operand type is not equal to right operand type! Left operand type: " ++ (tiName flti) ++ ". Right operand type: " ++ (tiName frti)) Nothing))
        else Right retTi

expectType :: Expression -> TypeInfo -> TypeInfo -> Either AnalysisError TypeInfo
expectType expr ex = expectTypes expr [ex]

expectTypes :: Expression -> [TypeInfo] -> TypeInfo -> Either AnalysisError TypeInfo
expectTypes expr exl ac = if elem ac exl
    then Right ac
    else Left (AnalysisError TypeMismatchError ("Expression: " ++ (show expr) ++ " has wrong type! Expected one of these types: " ++ (show (map tiName exl)) ++ ". Actual type: " ++ (tiName ac)) Nothing)

analyzeActualParams :: Analyzer -> [ParamInfo] -> [Expression] -> Bool -> Either AnalysisError Analyzer
analyzeActualParams a fps aps ignoreParamTypes = if fpsl /= apsl
    then Left (AnalysisError ActualParameterError ("Amounts of formal and actual parameters are different! FP amount: " ++ (show fpsl) ++ ", AP amount: " ++ (show apsl)) Nothing)
    else foldl analyzeParamType (Right a) (zip3 fps aps [1..])
    where fpsl = length fps
          apsl = length aps
          analyzeParamType a' p = case (a', p) of
            (Left er, _) -> Left er
            (a''@(Right _), (fp, ap, index)) -> case analyzeExpression a'' ap of
                Left er -> Left (AnalysisError ActualParameterError ("Wrong expression for actual parameter at position " ++ (show index) ++ "!") (Just er))
                Right ti -> if (ignoreParamTypes || (ti == paiType fp))
                    then a''
                    else Left (AnalysisError ActualParameterError ("Actual parameter at position " ++ (show index) ++ " has wrong type! Expected: " ++ (tiName (paiType fp)) ++ ". Actual: " ++ (tiName ti)) Nothing)

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

testSttmIf :: Statement
testSttmIf = If {
    iCondition = Val (Boolean True),
    iIfRoute = Assignment {aName = Identifier {idValue = "a"}, aValue = Val (IntNum 42) },
    iElseRoute = Just Assignment {aName = Identifier {idValue = "a"}, aValue = Val (IntNum 84) }
}

testSttmWhile :: Statement
testSttmWhile = While { wCondition = Val (IntNum 42), wBody = Compound []}