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
    -- ( r1, env ) <- catCommand env [ StringValue "Pljuska.cabal" ]
    -- putStrLn $ toString r1
    -- ( r2, env ) <- echoCommand env [ StringValue "Str", IntValue 10 ]
    -- putStrLn $ toString r2
    -- ( _, env ) <- cdCommand env [ StringValue "/home/sigma/Programming/Tmp" ]
    -- ( r3, env ) <- pwdCommand env []
    -- putStrLn $ toString r3
    -- ( _, env ) <- createCommand env [ StringValue "test.txt", StringValue "again.doc" ]
    -- ( r4, env ) <- lsCommand env []
    -- putStrLn $ toString r4
    -- ( _, env ) <- rmCommand env [ StringValue "test.txt", StringValue "again.doc" ]
    -- ( _, env ) <- mvCommand env [ StringValue "OOP/test.txt", StringValue "test.txt" ]
    -- ( r5, env ) <- hexdumpCommand env [ StringValue "pljuska.png" ]
    -- putStrLn $ toString r5
    -- ( _, env ) <- mkdirCommand env [ StringValue "test" ]
    -- ( _, env ) <- cdCommand env [ StringValue "test" ]
    -- ( _, env ) <- createCommand env [ StringValue "asdf.txt" ]
    -- ( r6, env ) <- lsCommand env []
    -- putStrLn $ toString r6
    -- ( _, env ) <- rmCommand env [ StringValue "test/asdf.txt" ]
    -- ( _, env ) <- mvdirCommand env [ StringValue "Test", StringValue "Else", StringValue "Dest" ]
    runAdditionalTests env
    putStrLn $ errorString "HASH Ending..."


runAdditionalTests :: Environment -> IO ()
runAdditionalTests env = do
    let lsCmd = BasicCommand "ls" [] Nothing Nothing False True
    let cdCmd = BasicCommand "cd" [ StaticData $ StringValue "Testing" ] Nothing Nothing False True
    let pwdCmd= BasicCommand "pwd" [] Nothing ( Just $ StaticData $ StringValue "pwd.txt" ) True True
    ( v1, env ) <- execute env lsCmd
    ( v2, env ) <- execute env cdCmd
    ( v3, env ) <- execute env lsCmd
    ( v4, env ) <- execute env pwdCmd
    --putStrLn $ toString v
    return ()
