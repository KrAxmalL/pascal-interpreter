module Interpreter(applyInterpreter, printInterpretationError) where

import Lexic
import Data.Map.Strict (Map, empty, insert, lookup)
import Text.Printf(printf)
import Data.Maybe

data InterpretationError = InterpretationError InterpretationErrorType String (Maybe InterpretationError) deriving (Show)
data InterpretationErrorType = WrongTypeError |
                               DivisionByZeroError
                               deriving (Show)

printInterpretationError :: InterpretationError -> String
printInterpretationError (InterpretationError tp message source) = initialMessage ++ sourceMessage ++ "]"
    where initialMessage = "InterpretationError-[type = " ++ (show tp) ++ ", message = " ++ message
          sourceMessage = case source of
            Nothing -> ""
            Just se -> ", source = " ++ (printInterpretationError se)

data Interpreter = I {
    callStack :: [ActivationRecord],
    currentSR :: ScopeRecord
} deriving (Show)

data ActivationRecord = AR {
    arName :: String,
    arLevel :: Int,
    arType :: RecordType,
    vars :: Map String VarInfo
} deriving (Show)

data ScopeRecord = SR {
    srName :: String,
    srLevel :: Int,
    srType :: RecordType,
    parameters :: Maybe [ParamInfo],
    srReturnType :: Maybe DataType,
    variables :: Map String VarInfo,
    functions :: Map String ScopeRecord,
    procedures :: Map String ScopeRecord,
    srBody :: Statement,
    parentSR :: Maybe ScopeRecord
}

instance Show ScopeRecord where
    show sr = printf "SR { srName = %s, srLevel = %d, srType = %s, parameters = %s, srReturnType = %s, variables = %s, functions = %s, procedures = %s, srBody = %s, parentSR = %s" (srName sr) (srLevel sr) (show (srType sr)) (show (parameters sr)) (show (srReturnType sr)) (show (variables sr)) (show (functions sr)) (show (procedures sr)) (show (srBody sr)) (show (fmap srName (parentSR sr)))

isGlobalScopeRecord :: ScopeRecord -> Bool
isGlobalScopeRecord sr = case parentSR sr of
    Nothing -> True
    _ -> False

data RecordType = RTProgram | RTFunction | RTProcedure deriving (Show)

data VarInfo = VI {viName :: String, viType :: DataType, viValue :: Maybe Value} deriving (Show)
data FuncInfo = FI {fiName :: String, fiParams :: [ParamInfo], fiResType :: DataType, fiBlock :: Block} deriving (Show)
data ProcInfo = PRI {priName :: String, priParams :: [ParamInfo], priBlock :: Block} deriving (Show)
data ParamInfo = PAI {paiName :: String, paiType :: DataType} deriving (Show)

updateVarInCallStack :: String -> Value -> [ActivationRecord] -> [ActivationRecord]
updateVarInCallStack varName val callStack =
    let (varInfo, pos) = findVarAndLocationInCallStack varName callStack
        ar = callStack !! pos
        (h, t) = splitAt pos callStack
        updatedAR = ar {vars = insert varName (varInfo {viValue = Just val}) (vars ar)}
    in (h ++ (updatedAR : (tail t)))

findVarInCallStack :: String -> [ActivationRecord] -> VarInfo
findVarInCallStack varName callStack = 
    let (vi, _) = findVarAndLocationInCallStack varName callStack
    in vi

findVarAndLocationInCallStack :: String -> [ActivationRecord] -> (VarInfo, Int)
findVarAndLocationInCallStack varName callStack = findVar varName (zip callStack [0..])
    where findVar varName' ars = case (Data.Map.Strict.lookup varName' (vars (fst (head ars)))) of
                                            Just v -> (v, (snd (head ars)))
                                            Nothing -> findVar varName' (tail ars)

findFunctionSR :: String -> ScopeRecord -> ScopeRecord
findFunctionSR fnName sr = 
    if fnName == (srName sr) 
    then sr
    else case Data.Map.Strict.lookup fnName (functions sr) of
    Just functionSR -> functionSR
    Nothing -> case parentSR sr of
        Just psr -> findFunctionSR fnName psr
        Nothing -> error "Function must exist"

findProcedureSR :: String -> ScopeRecord -> ScopeRecord
findProcedureSR prName sr = 
    if prName == (srName sr) 
    then sr
    else case Data.Map.Strict.lookup prName (procedures sr) of
    Just procedurePR -> procedurePR
    Nothing -> case parentSR sr of
        Just psr -> findProcedureSR prName psr
        Nothing -> error "Procedure must exist"

applyInterpreter :: Program -> IO (Either InterpretationError Interpreter)
applyInterpreter p = interpretStatement (pure (Right interpreter)) programBody
    where sr = buildProgramScopeRecord p
          programBody = srBody sr
          initialVars = foldl (\varMap varInfo -> insert (viName varInfo) varInfo varMap) empty (variables sr)
          interpreter = I {callStack = [AR {
                arName = srName sr,
                arLevel = 1,
                arType = RTProgram,
                vars = initialVars
            }], currentSR = sr}

buildProgramScopeRecord :: Program -> ScopeRecord
buildProgramScopeRecord Program {pHeader, pBody} = buildBlockScopeRecord (idValue pHeader) 1 RTProgram Nothing Nothing pBody

buildBlockScopeRecord :: String -> Int -> RecordType -> Maybe [ParamInfo] -> Maybe DataType -> Block -> ScopeRecord
buildBlockScopeRecord nm lvl rt pms rett Block {bDeclarations, bBody} = 
    let initialSR = SR {
        srName = nm, 
        srLevel = lvl, 
        srType = rt, 
        parameters = pms, 
        srReturnType = rett, 
        variables = empty, 
        functions = empty, 
        procedures = empty, 
        srBody = bBody,
        parentSR = Nothing}
    in  foldl buildDeclarationScopeRecord initialSR bDeclarations 

buildDeclarationScopeRecord :: ScopeRecord -> Declaration -> ScopeRecord
buildDeclarationScopeRecord sr (VarDecl varDecls) = foldl interpretVar sr varDecls
    where interpretVar sr' vd =
            let varName = idValue (vName vd)
                varType = vType vd
                newVI = VI {viName = varName, viType = varType, viValue = vValue vd}
            in sr' {variables = insert varName newVI (variables sr')}
buildDeclarationScopeRecord sr (FuncDecl fn) =
    let fnName = idValue (fName fn)
        lvl = (srLevel sr) + 1
        pms = Just (map (\p -> PAI {paiName = (idValue (fpName p)), paiType = fpType p}) (fParams fn))
        retType = Just (fResType fn)
        body = fBlock fn
        functionSR = buildBlockScopeRecord fnName lvl RTFunction pms retType body
        updatedSR = sr {
            functions = insert fnName (functionSR {parentSR = Just updatedSR}) (functions sr)
        }
    in updatedSR
buildDeclarationScopeRecord sr (ProcDecl pr) =
    let prName = idValue (pName pr)
        lvl = (srLevel sr) + 1
        pms = Just (map (\p -> PAI {paiName = (idValue (fpName p)), paiType = fpType p}) (pParams pr))
        retType = Nothing
        body = pBlock pr
        procedureSR = buildBlockScopeRecord prName lvl RTProcedure pms retType body
        updatedSR = sr {
            procedures = insert prName (procedureSR {parentSR = Just updatedSR}) (procedures sr)
        }
    in updatedSR

interpretStatement :: IO (Either InterpretationError Interpreter) -> Statement -> IO (Either InterpretationError Interpreter)
interpretStatement ioi sttm = do 
    intr <- ioi
    case (intr, sttm) of
        (Left _, _) -> return intr
        (Right i, Assignment {aName, aValue}) -> do
            res <- interpretExpression i aValue
            return (case res of
                Left er -> Left er
                Right (i', v) -> Right (i' {callStack = updateVarInCallStack (idValue aName) v (callStack i')}))
        (Right i, ProcCall {pcName, pcParams}) -> do
            res <- interpretParams i pcParams
            case res of
                Left er -> pure (Left er)
                Right (i', parameterValues) -> 
                    case (idValue pcName) of
                         "write" -> do
                                      putStr (foldl (++) "" (map printValue parameterValues))
                                      return (Right (i'))
                         "writeln" -> do
                                      putStrLn (foldl (++) "" (map printValue parameterValues))
                                      return (Right (i'))
                         pcNameStr -> 
                            let currSR = currentSR i'
                                procedureSR = findProcedureSR pcNameStr (currentSR i')
                                paramVars = buildParameterMap (fromJust (parameters procedureSR)) parameterValues
                                declaredVars = foldl (\varMap varInfo -> insert (viName varInfo) varInfo varMap) paramVars (variables procedureSR)
                                procedureAR = AR {
                                    arName = (srName procedureSR),
                                    arLevel = arLevel (head (callStack i')),
                                    arType = srType procedureSR,
                                    vars = declaredVars
                                }
                            in do
                                 res' <- interpretStatement (pure (Right i' {currentSR = procedureSR, callStack = procedureAR : (callStack i')})) (srBody procedureSR)
                                 return (case res' of
                                     Left er -> Left er
                                     Right i'' -> Right (i'' {currentSR = currSR, callStack = tail (callStack i'')}))
        (Right _, Compound sttms) -> foldl interpretStatement ioi sttms
        (Right i, If {iCondition, iIfRoute, iElseRoute}) -> do
            res <- interpretExpression i iCondition
            case res of
                Left er -> pure (Left er)
                Right (i', v) -> case expectBooleanType v of
                    Left er -> pure (Left er)
                    Right (Boolean True) -> interpretStatement (pure (Right i')) iIfRoute
                    Right (Boolean False) -> case iElseRoute of
                            Just sttm -> interpretStatement (pure (Right i')) sttm
                            Nothing -> pure (Right i')
        (Right i, whileSttm@(While {wCondition, wBody})) -> do
            res <- interpretExpression i wCondition
            case res of
                Left er -> pure (Left er)
                Right (i', v) -> case expectBooleanType v of
                    Left er -> pure (Left er)
                    Right (Boolean False) -> pure (Right i')
                    Right (Boolean True) -> do
                        res' <- interpretStatement (pure (Right i')) wBody
                        case res' of
                            Left er -> pure (Left er)
                            Right i'' -> interpretStatement (pure (Right i'')) whileSttm

interpretExpression :: Interpreter -> Expression -> IO (Either InterpretationError (Interpreter, Value))
interpretExpression i (Val v) = pure (Right (i, v))
interpretExpression i (VarRef iden) = pure (case varValue of
    Just v -> Right (i, v)
    Nothing -> Right (i, getDefaultValue varType))
    where vi = findVarInCallStack (idValue iden) (callStack i)
          varType = viType vi
          varValue = viValue vi
interpretExpression i (UnOp unOp expr) = do 
    res <- interpretExpression i expr
    return (case res of
        Left er -> Left er
        Right (i', v) -> case unOp of
            Not -> case expectBooleanType v of
                Left er -> Left er
                Right (Boolean b) -> Right (i', Boolean (not b))
            UnaryPlus -> case expectIntType v of
                Left er -> Left er
                Right (IntNum n) -> Right (i', IntNum n)
            UnaryMinus -> case expectIntType v of
                Left er -> Left er
                Right (IntNum n) -> Right (i', IntNum (-n)))
interpretExpression i (BinOp {boOp, boLeft, boRight}) = do 
    resl <- interpretExpression i boLeft
    case resl of
        Left er -> pure (Left er)
        Right (i', lv) -> case boOp of
            And -> case lv of
                Boolean False -> pure (Right (i', Boolean False))
                Boolean lvv@True -> do 
                    resr <- interpretExpression i' boRight
                    return (case resr of
                        Left er -> Left er
                        Right (i'', Boolean rvv) -> Right (i'', Boolean (lvv && rvv))
                        Right _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing))
                _ -> pure (Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing))
            Or -> case lv of
                Boolean True -> pure (Right (i', Boolean True))
                Boolean lvv@False -> do 
                    resr <- interpretExpression i' boRight
                    return (case resr of
                        Left er -> Left er
                        Right (i'', Boolean rvv) -> Right (i'', Boolean (lvv || rvv))
                        Right _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing))
                _ -> pure (Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing))
            _ -> do 
                    resr <- interpretExpression i' boRight
                    return (case resr of
                        Left er -> Left er
                        Right (i'', rv) -> case boOp of
                            Plus -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> Right (i'', IntNum (lvv + rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            Minus -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> Right (i'', IntNum (lvv - rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            Mul -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> Right (i'', IntNum (lvv * rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            Div -> case (lv, rv) of -- TODO: use '/' and return Double type
                                (IntNum lvv, IntNum rvv) -> if rvv == 0 
                                                            then Left (InterpretationError DivisionByZeroError ("Can't use '" ++ (show boOp) ++ "' operator - division by zero") Nothing)
                                                            else Right (i'', IntNum (div lvv rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            FullDiv -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> if rvv == 0 
                                                            then Left (InterpretationError DivisionByZeroError ("Can't use '" ++ (show boOp) ++ "' operator - division by zero") Nothing)
                                                            else Right (i'', IntNum (div lvv rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            Mod -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> if rvv == 0 
                                                            then Left (InterpretationError DivisionByZeroError ("Can't use '" ++ (show boOp) ++ "' operator - division by zero") Nothing)
                                                            else Right (i'', IntNum (mod lvv rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            Eql -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> Right (i'', Boolean (lvv == rvv))
                                (Boolean lvv, Boolean rvv) -> Right (i'', Boolean (lvv == rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            Neql -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> Right (i'', Boolean (lvv /= rvv))
                                (Boolean lvv, Boolean rvv) -> Right (i'', Boolean (lvv /= rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            Gt -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> Right (i'', Boolean (lvv > rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            Gte -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> Right (i'', Boolean (lvv >= rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            Lt -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> Right (i'', Boolean (lvv < rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing) 
                            Lte -> case (lv, rv) of
                                (IntNum lvv, IntNum rvv) -> Right (i'', Boolean (lvv <= rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing)
                            Xor -> case (lv, rv) of
                                (Boolean lvv, Boolean rvv) -> Right (i'', Boolean (lvv /= rvv))
                                _ -> Left (InterpretationError WrongTypeError ("Wrong value type in '" ++ (show boOp) ++ "' operator!") Nothing))
interpretExpression i (FuncCall {fcName, fcParams}) = do 
    res <- interpretParams i fcParams
    case res of
        Left er -> pure (Left er)
        Right (i', parameterValues) ->
            let currSR = currentSR i'
                functionName = idValue fcName
                functionSR = findFunctionSR (idValue fcName) (currentSR i')
                paramVars = buildParameterMap (fromJust (parameters functionSR)) parameterValues
                declaredVars = foldl (\varMap varInfo -> insert (viName varInfo) varInfo varMap) paramVars (variables functionSR)
                finalVars = insert functionName (VI {viName = functionName, viType = fromJust (srReturnType functionSR), viValue = Nothing}) declaredVars
                functionAR = AR {
                    arName = (srName functionSR),
                    arLevel = arLevel (head (callStack i')),
                    arType = srType functionSR,
                    vars = finalVars
                }
            in do 
                res' <- interpretStatement (pure (Right i' {currentSR = functionSR, callStack = functionAR : (callStack i')})) (srBody functionSR)
                return (case res' of
                    Left er -> Left er
                    Right i'' -> Right (i'' {currentSR = currSR, callStack = tail (callStack i'')}, fromJust (viValue (findVarInCallStack functionName (callStack i'')))))
interpretExpression i (Paren expr) = interpretExpression i expr

interpretParams :: Interpreter -> [Expression] -> IO (Either InterpretationError (Interpreter, [Value]))
interpretParams i = foldl interpretParam (pure (Right (i, [])))
    where interpretParam ioi p = do
            i' <- ioi
            case i' of
                Left er -> pure (Left er)
                Right (i'', l) -> do 
                    i''' <- interpretExpression i'' p
                    return (case i''' of 
                        Left er -> Left er
                        Right (i''', v) -> Right (i''', v : l))

buildParameterMap :: [ParamInfo] -> [Value] -> Map String VarInfo
buildParameterMap params values = foldl (\paramMap (paramInfo, paramValue) -> insert (paiName paramInfo) (VI {viName = paiName paramInfo, viType = paiType paramInfo, viValue = Just paramValue}) paramMap) empty (zip params values)

expectIntType :: Value -> Either InterpretationError Value
expectIntType = expectType "integer"

expectBooleanType :: Value -> Either InterpretationError Value
expectBooleanType = expectType "boolean"

expectType :: String -> Value -> Either InterpretationError Value
expectType t v = case t of
    "boolean" -> case v of
                Boolean _ -> Right v
                _ -> typeError
    "integer" -> case v of
        IntNum _ -> Right v
        _ -> typeError
    _ -> error "unreachable"
    where typeError = Left (InterpretationError WrongTypeError ("Wrong value type! Expected type: '" ++ t ++ "'!") Nothing)

getDefaultValue :: DataType -> Value
getDefaultValue DTInteger = IntNum 0
getDefaultValue DTBoolean = Boolean False