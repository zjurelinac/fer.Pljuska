module Hash(
    runInteractive,
    runScript,
    runTest
) where

import Language.Core
import Language.Definitions
import Language.Commands.Basic
import Language.Commands.Filesystem

import Interpreter
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
    runAdditionalTests env
    putStrLn $ errorString "HASH Ending..."


runAdditionalTests :: Environment -> IO ()
runAdditionalTests env = do
    return ()
