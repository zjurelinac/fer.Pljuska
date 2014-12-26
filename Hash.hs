module Hash(
    runInteractive,
    runScript,
    runTest
) where

import Control.Exception

import Language.Core
import Language.Definitions
import Language.Commands.Basic
import Language.Commands.Filesystem

import Interpreter
import Parsing.Tokenizer
import Utility.Console
import Utility.Data
import Utility.Terminal

runInteractive :: IO ()
runInteractive = print "Hash interactive"


runScript :: FilePath -> IO ()
runScript fp = do
    file <- readFile fp
    print file


startup :: Environment -> IO ()
startup env = do
    setTermTitle
    displayStartScreen
    displayPrompt env


runTest :: IO ()
runTest = do
    env <- blankEnvironment
    startup env
    r <- runAdditionalTests env `catches` [ Handler handleUserEx, Handler handleIOEx ]
    putStrLn $ errorString "HASH Ending..."

handleUserEx :: ErrorCall -> IO ()
handleUserEx e = putStrLn . errorString $ "E: " ++ show e

handleIOEx :: IOException -> IO ()
handleIOEx e = putStrLn . errorString . show $ e

runAdditionalTests :: Environment -> IO ()
runAdditionalTests env = do
    a <- getLine
    putStrLn . show . tokenizeString $ a
