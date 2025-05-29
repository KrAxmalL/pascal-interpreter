module Main (main) where

import Analyzer
import Interpreter
import Parser
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  if (length args < 1)
    then putStrLn "ERROR: Command was not provided"
    else executeCommand (head args) (tail args)

executeCommand :: String -> [String] -> IO ()
executeCommand "run" args = executeRunCommand args
executeCommand cmd _ = putStrLn ("ERROR: Unknown command '" ++ cmd ++ "'")

executeRunCommand :: [String] -> IO ()
executeRunCommand args =
  if (length args < 1)
    then putStrLn "ERROR: expecting file path to run the program"
    else
      let
        filePath : flags = args
        isDebug = (length flags > 0) && (head flags == "--debug")
       in
        runInterpreter filePath isDebug

runInterpreter :: String -> Bool -> IO ()
runInterpreter filePath isDebug = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  putStrLn ("Interpreting source file: '" ++ filePath ++ "'...")
  putStrLn ""
  ifDebug (putStrLn "Parsing program...")
  case applyParser filePath contents of
    Left er -> putStrLn (show er)
    Right pr -> do
      ifDebug (putStrLn (show pr))
      ifDebug printSeparator
      ifDebug (putStrLn "Analyzing parsed program...")
      case applyAnalyzer pr of
        Left er -> putStrLn (show er)
        Right a -> do
          ifDebug (putStrLn (show a))
          ifDebug printSeparator
          ifDebug (putStrLn "Interpreting program...")
          ifDebug (putStrLn "Program output:")
          intrRes <- applyInterpreter pr
          case intrRes of
            Left er -> putStrLn (show er)
            Right i -> do
              ifDebug (putStrLn "")
              ifDebug printSeparator
              ifDebug (putStrLn "Final interpreter state:")
              ifDebug (putStrLn (show i))
  hClose handle
 where
  ifDebug fn = if isDebug then fn else pure ()
  printSeparator = putStrLn (take 40 $ repeat '-')