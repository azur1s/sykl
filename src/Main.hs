module Main where

import System.Environment (getArgs)
import System.Exit
import Parser
import VM
import Commands

help :: IO ()
help = putStrLn "Usage: skyl <file>"

version :: IO ()
version = putStrLn "0.1.0"

parseArgs :: [String] -> IO String
parseArgs ["-h"] = help >> exitWith ExitSuccess
parseArgs ["-v"] = version >> exitWith ExitSuccess
parseArgs []     = getContents
parseArgs fs     = concat `fmap` mapM readFile fs

main :: IO ()
main = do
    args <- getArgs
    source <- parseArgs args
    case parse source of
        Left e -> putStrLn $ "Parse Error: " ++ show e
        Right ins -> do
            let vm = initVM ins
            let vm' = exec vm
            putStr $ finalize vm'