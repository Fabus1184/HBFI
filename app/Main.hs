module Main (main) where

import Lib (eval)

import Control.Monad (void)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    k <- getArgs >>= parseArgs
    c <- readFile k
    void $ eval ((mempty, 0), c)
  where
    parseArgs [] = parseArgs ["--help"]
    parseArgs ("--help" : xs) = do
        putStrLn $
            intercalate
                "\n"
                [ "Usage:"
                , "\thbfi File"
                , ""
                , "Options:"
                , "\t--help\t\t\tshow this message"
                ]
        exitSuccess
    parseArgs [f] = pure f
    parseArgs xs = do
        hPutStrLn stderr $ "Unrecognized options: " ++ intercalate "," xs
        exitFailure
