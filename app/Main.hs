module Main (main) where

import System.IO
import Parser

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
    putStrLn (show (applyParser fileName contents))
    hClose handle

tokenize :: String -> [String]
tokenize s = lines s >>= words
