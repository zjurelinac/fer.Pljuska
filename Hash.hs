module Hash(
    runInteractive,
    runScript
) where

import Control.Exception hiding ( evaluate )
import Control.Monad
import System.Exit

import Language.Core
import Language.Definitions
import Language.Commands.Basic
import Language.Commands.Filesystem

import Interpreter
import Parsing.Tokenizer
import Parsing.Parser
import Utility.Console
import Utility.Data
import Utility.File
import Utility.Terminal


runInteractive :: IO ()
runInteractive = do
    env <- blankEnvironment
    startup env
    runOnce env
    return ()


runOnce :: Environment -> IO ()
runOnce env = do
    displayPrompt env
    l <- getLine
    let c = parseInput l
    tenv <- try ( runBlock env c ) :: IO ( Either SomeException Environment )
    case tenv of
        Left e      -> procError e >> runOnce env
        Right env'  -> runOnce env'
    return ()


runScript :: FilePath -> IO ()
runScript fp = do
    setInitialDirectory fp
    env <- blankEnvironment
    runScript' ( getBaseName fp ) env
    return ()


runScript' :: FilePath -> Environment -> IO ()
runScript' fp env = do
    s <- readFile fp
    let c = parseInput s
    _ <- execute env c
    return ()


startup :: Environment -> IO ()
startup env = do
    setTermTitle
    displayStartScreen
    displayDivider
    return ()


procError :: SomeException -> IO ()
procError e = do
    let er = show e
    if er == "ExitSuccess" then exitSuccess else return ()
    let er' = if ( take 5 er ) == "Map.!" then "No such command exists" else er
    putStrLn . errorString $ er'
    return ()

