module Main (main) where

import Lib (eval)

import Control.Monad (void)
import Control.Monad.State (evalStateT)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    k <- getArgs >>= parseArgs
    c <- readFile k
    void $ evalStateT (eval c) (pure 0)
  where
    parseArgs [] = parseArgs ["--help"]
    parseArgs ("--help" : xs) = do
        putStrLn $
            intercalate
                "\n"
                [ "Usage:"
                , "\thbfi File [options]"
                , ""
                , "Options:"
                , "\t--help\t\t\tshow this message"
                ]
        exitSuccess
    parseArgs [f] = pure f
    parseArgs [f, "--debug"] = pure f
    parseArgs xs = do
        hPutStrLn stderr $ "Unrecognized option(s): " ++ intercalate "," xs
        exitFailure
