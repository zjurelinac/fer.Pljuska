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
    -- let toks = [ BlockStart,
    --              ControlToken "if", TestStart, VariableToken "a", EqualToken, IntToken 1, TestEnd,
    --              BlockStart, EndToken,
    --              CommandToken "ls", ParameterToken "-la", EndToken,
    --              BlockEnd, ControlToken "else", BlockStart, EndToken,
    --              CommandToken "pwd", EndToken,
    --              BlockEnd, EndToken,
    --              CommandToken "ls", EndToken,
    --              VariableToken "a", AssignToken, IntToken 1, BinaryPlusToken, VariableToken "a", EndToken,
    --              BlockEnd, EndToken ]
    let toks = [ BlockStart,
                 VariableToken "a", AssignToken, IntToken 1, EndToken,
                 ControlToken "while", TestStart, VariableToken "a", LesserEqualToken, IntToken 4, TestEnd, BlockStart, EndToken,
                 CommandToken "echo", VariableToken "a", EndToken,
                 VariableToken "a", AssignToken, VariableToken "a", BinaryPlusToken, IntToken 1, EndToken,
                 BlockEnd, EndToken,
                 BlockEnd, EndToken ]
    let a = parseBasicBlock toks
    print a
    (x, y ) <- execute env ( fst a )
    putStr ""
