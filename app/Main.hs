module Main (main) where

import System.IO
import Parser
import Analyzer

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
parseProgram = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    case applyParser fileName contents of
        Left er -> putStrLn (show er)
        Right pr -> do
            putStrLn (show pr)
            case applyAnalyzer pr of
                Left er -> putStrLn (show er)
                Right a -> putStrLn (show a)
    hClose handle

tokenize :: String -> [String]
tokenize s = lines s >>= words
