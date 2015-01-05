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
    -- let s = "10 - 2*(-1) - 3*7/11"
    -- let t = tokenizeInput s
    -- putStrLn . show $ t
    -- let a = parseArithmetic t
    -- putStrLn . show $ a
    -- let r = evaluate env ( fst a )
    -- print r
    --let toks = [ VariableToken "a", AssignToken, CommandToken "ls", EndToken, CommandToken "cd", ParameterToken "folder", EndToken ]
    let toks = [ BlockStart,
                 ControlToken "if", TestStart, VariableToken "a", EqualToken, IntToken 1, TestEnd,
                 BlockStart, EndToken,
                 CommandToken "ls", ParameterToken "-la", EndToken,
                 BlockEnd, ControlToken "else", BlockStart, EndToken,
                 CommandToken "pwd", EndToken,
                 BlockEnd, EndToken,
                 CommandToken "ls", EndToken,
                 BlockEnd, EndToken ]
    --let ( cond, rest )  = parseCondition $ tail toks
    --let r2@( b1, rest1 )   = parseBasicBlock rest
    --putStrLn . show $ r2
    --let ts' = drop 1 $ take 6 toks
    --putStrLn . show $ ts'
    --let c = parseCondition ts'
    --putStrLn . show $ c
    let a = parseBasicBlock toks
    print a
    putStr ""
