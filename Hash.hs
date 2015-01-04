module Hash(
    runInteractive,
    runScript,
    runTest
) where

import Control.Exception hiding ( evaluate )

import Language.Core
import Language.Definitions
import Language.Commands.Basic
import Language.Commands.Filesystem

import Interpreter
import Parsing.Tokenizer
import Parsing.Parser
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
    putStrLn ""


handleUserEx :: ErrorCall -> IO ()
handleUserEx e = putStrLn . errorString $ "E: " ++ show e


handleIOEx :: IOException -> IO ()
handleIOEx e = putStrLn . errorString . show $ e


runAdditionalTests :: Environment -> IO ()
runAdditionalTests env = do
    a <- getLine
    let b = tokenizeString a
    putStrLn . show $ b
    let c = convertToRPN ( tail $ init b )
    putStrLn . show $ c
    let d = parseCondition $ preprocessCond [] c
    putStrLn . show $ d
    --let c = parseCommand False b
    --putStrLn . show $ c
    --putStrLn . show . tokenizeString $ a
    --( r, env ) <- execute env c
    {-let ts = [ IntToken 3, BinaryPlusToken, IntToken 4, MultiplyToken, IntToken 2, DivideToken, LeftParens, IntToken 1, BinaryMinusToken, IntToken 5, RightParens ]
    let cs = convertToRPN ts
    let ps = parseArithmetic cs
    print cs
    print ps
    print $ evaluate env ps-}
    putStr ""
