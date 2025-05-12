module Main (main) where

import System.IO
import Parser
import Analyzer
import Interpreter

fileName :: String
fileName = "./sources/program.pas"

main :: IO ()
main = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    putStrLn (let tokens = tokenize contents
                  str = map show tokens
                  in foldl (++) "" str)
    hClose handle

parseProgram :: IO ()
parseProgram = runInterpreter fileName

runInterpreter :: String -> IO ()
runInterpreter filePath = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    putStrLn ("Parsing source file: '" ++ filePath ++ "'...")
    case applyParser filePath contents of
        Left er -> putStrLn (show er)
        Right pr -> do
            putStrLn (show pr)
            putStrLn (take 40 $ repeat '-')
            putStrLn "Analyzing parsed program..."
            case applyAnalyzer pr of
                Left er -> putStrLn (printAnalysisError er)
                Right a -> do
                    putStrLn (show a)
                    putStrLn (take 40 $ repeat '-')
                    putStrLn "Interpreting program..."
                    intrRes <- applyInterpreter pr
                    case intrRes of
                        Left er -> putStrLn (printInterpretationError er)
                        Right i -> putStrLn (show i)
    hClose handle

tokenize :: String -> [String]
tokenize s = lines s >>= words
