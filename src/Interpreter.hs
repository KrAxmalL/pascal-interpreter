module Interpreter(applyInterpreter, printInterpretationError) where

import Lexic
import Data.Map.Strict (Map, empty, insert)

data InterpretationError = InterpretationError InterpretationErrorType String (Maybe InterpretationError) deriving (Show)
data InterpretationErrorType = DivisionByZeroError
                     deriving (Show)

printInterpretationError :: InterpretationError -> String
printInterpretationError (InterpretationError tp message source) = initialMessage ++ sourceMessage ++ "]"
    where initialMessage = "InterpretationError-[type = " ++ (show tp) ++ ", message = " ++ message
          sourceMessage = case source of
            Nothing -> ""
            Just se -> ", source = " ++ (printInterpretationError se)

data Interpreter = I {
    callStack :: [ActivationRecord],
    currentSR :: ScopeRecord,
    globalSR :: ScopeRecord
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
    variables :: Map String VarInfo,
    functions :: Map String ScopeRecord,
    procedures :: Map String ScopeRecord,
    srBody :: Statement,
    parentSR :: Maybe ScopeRecord
} deriving (Show)

isGlobalScopeRecord :: ScopeRecord -> Bool
isGlobalScopeRecord sr = case parentSR sr of
    Nothing -> True
    _ -> False

data RecordType = Prg | Function | Procedure deriving (Show)

data VarInfo = VI {viName :: String, viType :: String, viValue :: Maybe Value} deriving (Show)
data FuncInfo = FI { fiName :: String, fiParams :: [ParamInfo], fiResType :: String, fiBlock :: Block} deriving (Show)
data ProcInfo = PRI { priName :: String, priParams :: [ParamInfo], priBlock :: Block} deriving (Show)
data ParamInfo = PAI {paiName :: String, paiType :: String} deriving (Show)

startingInterpreter :: Interpreter
startingInterpreter = I {callStack = []}

applyInterpreter :: Program -> Either InterpretationError Interpreter
applyInterpreter p = interpretProgram startingInterpreter p

interpretProgram :: Interpreter -> Program -> Either InterpretationError Interpreter
interpretProgram i Program {pHeader, pBody} = interpretBlock (Right i {callStack = AR {arName = (idValue pHeader), arType = Prg, arLevel = 1, vars = empty} : (callStack i), currentSR = newScopeRecord, globalSR = newScopeRecord}) pBody
    where newScopeRecord = SR {srName = (idValue pHeader), srLevel = 1, variables = empty, functions = empty, procedures = empty, srBody = pBody, parentSR = Nothing}

interpretBlock :: Either InterpretationError Interpreter -> Block -> Either InterpretationError Interpreter
interpretBlock i@(Left _) _ _ = i
interpretBlock i@(Right _) Block {bDeclarations, bBody} = foldl interpretDeclaration i bDeclarations

interpretDeclaration :: Either InterpretationError Interpreter -> Declaration -> Either InterpretationError Interpreter
interpretDeclaration i@(Left _) _ = i
interpretDeclaration i@(Right _) (VarDecl varDecls) = foldl interpretVar i varDecls
    where interpretVar i' v =
            let varName = idValue (vName v)
                varType = idValue (vType v) in case i' of
                    Left er -> Left er
                    Right i'' -> let currAR = head (callStack i'')
                                     newVI = VI {viName = varName, viType = varType, viValue = vValue v}
                                     updatedCurrAR = currAR {vars = insert varName newVI (vars currAR)}
                                     updatedCallStack = updatedCurrAR : (tail (callStack i''))
                                     currSR = currentSR i''
                                     updatedCurrentSR = currSR {variables = insert varName newVI (variables currSR)}
                                     updatedInterpreter = i'' {callStack = updatedCallStack, currentSR = updatedCurrentSR, globalSR = if (isGlobalScopeRecord updatedCurrentSR) then updatedCurrentSR else (globalSR i'')}
                                  in Right updatedInterpreter
interpretDeclaration (Right i) (FuncDecl fn) = case interpretBlock (Right i {currentSR = nestedSR}) (fBlock fn) of
    Left er -> Left er
    Right i' -> Right (i' {currentSR i' {functions = insert (functions)}})
        let fnName = idValue (fName fn)
            currSR = currentSR i
            newSR = 
                FI { fiName = fnName, fiParams = map (\p -> PAI {paiName = (idValue (fpName p)), paiType = idValue (fpType p)}) (fParams fn), fiResType = idValue (fResType fn), fiBlock = (fBlock fn)}
            updatedCurrentSR = currSR {functions = insert fnName currSR (functions currSR)}
            updatedInterpreter = i {currentSR = updatedCurrentSR, globalSR = if (isGlobalScopeRecord updatedCurrentSR) then updatedCurrentSR else (globalSR i)}
        in Right updatedInterpreter
    where nestedSR = SR {
        srName = fnName,
        srLevel = (srLevel currSR) + 1,
        srType = Function,
        parameters = Just (map (\p -> PAI {paiName = (idValue (fpName p)), paiType = idValue (fpType p)}) (fParams fn)),
        variables = empty,
        functions = empty,
        procedures = empty,
        parentSR = currSR
    }

-- interpretDeclaration (Right i) (ProcDecl pr) = case (findInScope (currentScope i) prName) of -- 1. Verify procedure name is available
--                 Just er -> Left (AnalysisError ProcedureDeclarationError ("Error while declaring procedure '" ++ prName ++ "'!") (Just er))
--                 Nothing -> let currScope = currentScope i
--                                newScope = createNestedScope currScope
--                            in case (analyzeFormalParamList newScope (pParams pr)) of -- 2. Verify procedure parameters have unique names and valid types
--                                 Left er -> Left (AnalysisError ProcedureDeclarationError ("Error while declaring procedure '" ++ prName ++ "'!") (Just er))
--                                 Right analyzedNewScope -> let updatedParentScope = currScope {procedures = ST ((prName, PRI { priName = prName, priParams = map (\p -> PAI {paiName = (idValue (fpName p)), paiType = getRight (getType currScope (idValue (fpType p)))}) (pParams pr)}) : (stTable (procedures currScope)))}
--                                                               finalNewScope = analyzedNewScope {parentScope = Just updatedParentScope}
--                                                               updatedAnalyzer = Right i {globalScope = if (isGlobalScope updatedParentScope) then updatedParentScope else (globalScope i), currentScope = finalNewScope}
--                                                           in case analyzeBlock updatedAnalyzer (pBlock pr) of
--                                                             Left er -> Left (AnalysisError ProcedureDeclarationError ("Error while declaring procedure '" ++ prName ++ "'!") (Just er))
--                                                             Right i' -> Right i' {currentScope = fromJust (parentScope (currentScope i'))}
--             where prName = idValue $ pName pr
interpretDeclaration i@(Right _) _ = i
-- 
-- interpretFormalParamList :: Scope -> [FormalParam] -> Either AnalysisError Scope
-- interpretFormalParamList sc = foldl interpret (Right sc)
-- 
-- interpretFormalParam :: Either AnalysisError Scope -> FormalParam -> Either AnalysisError Scope
-- interpretFormalParam sc p = case sc of
--             Left er -> Left er
--             Right sc' -> case (findInScope sc' paramName) of
--                 Just er -> Left (AnalysisError FormalParameterDeclarationError ("Error while declaring formal parameter '" ++ paramName ++ "'!") (Just er))
--                 Nothing -> case (getType sc' paramType) of
--                     Left er -> Left (AnalysisError FormalParameterDeclarationError ("Error while declaring formal parameter '" ++ paramName ++ "'!") (Just er))
--                     Right typeInfo -> Right sc' {variables = ST ((paramName, VI {viName = paramName, viType = typeInfo}) : (stTable (variables (sc'))))}
--             where paramName = idValue (fpName p)
--                   paramType = idValue (fpType p)