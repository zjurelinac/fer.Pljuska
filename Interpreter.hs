module Interpreter where

import Data.Maybe
import qualified Data.Map as M

import System.Directory
import System.Environment

import Language.Core
import Language.Definitions
import Language.Commands.Basic
import Language.Commands.Filesystem

import Utility.File


defaultCommands :: CommandList
defaultCommands = M.fromList [
                ( "cat",        catCommand      ),
                ( "cd",         cdCommand       ),
                ( "cp",         cpCommand       ),
                ( "cpdir",      cpdirCommand    ),
                ( "create",     createCommand   ),
                ( "dir",        lsCommand       ),
                ( "echo",       echoCommand     ),
                ( "exit",       exitCommand     ),
                ( "grep",       grepCommand     ),
                ( "hexdump",    hexdumpCommand  ),
                ( "ls",         lsCommand       ),
                ( "mkdir",      mkdirCommand    ),
                ( "mv",         mvCommand       ),
                ( "mvdir",      mvdirCommand    ),
                ( "panic",      panicCommand    ),
                ( "pwd",        pwdCommand      ),
                ( "rm",         rmCommand       ),
                ( "rmdir",      rmdirCommand    ),
                ( "quit",       exitCommand     )
            ]



-- Create a new, blank environment, from the current directory
blankEnvironment :: IO Environment
blankEnvironment = do
    fp <- getCurrentDirectory
    h <- getEnv "HOME"
    u <- getEnv "USER"
    return $ Environment {
        commandList         = defaultCommands,
        currentDirectory    = fp,
        lastReturn          = NoValue,
        variables           = M.fromList [
            ( "HOME",           StringValue h ),
            ( "CONF",           StringValue $ h ++ "/.hashrc" ),
            ( "USER",           StringValue u ),
            ( "BASEPATH",       StringValue "/home/sigma/Programming/Haskell/Projects/Pljuska" )
        ] }


-- Set initial directory
setInitialDirectory :: FilePath -> IO ()
setInitialDirectory x = do
    let d = getParentPath x
    if null d then return ()
        else setCurrentDirectory d

