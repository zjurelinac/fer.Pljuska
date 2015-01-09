module Interpreter where

import Data.Maybe
import qualified Data.Map as M

import System.Directory

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
    return $ Environment {
        commandList         = defaultCommands,
        currentDirectory    = fp,
        lastReturn          = NoValue,
        variables           = M.fromList [
            ( "HOME",           StringValue "/home/sigma"     ),
            ( "BASEPATH",       StringValue "/home/sigma/Programming/Haskell/Projects/Pljuska" )
        ] }


-- Set initial directory
setInitialDirectory :: FilePath -> IO ()
setInitialDirectory x = do
    let d = getParentPath x
    setCurrentDirectory d


