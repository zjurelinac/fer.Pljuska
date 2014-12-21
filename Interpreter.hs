module Interpreter where

import Data.Maybe
import qualified Data.Map as M

import System.Directory

import Language.Core
import Language.Definitions
import Language.Commands.Basic
import Language.Commands.Filesystem


defaultCommands :: CommandList
defaultCommands = M.fromList [
                ( "cat",        catCommand      ),
                ( "cd",         cdCommand       ),
                ( "cp",         cpCommand       ),
                --( "cpdir",      cpdirCommand    ),
                ( "create",     createCommand   ),
                ( "dir",        lsCommand       ),
                ( "echo",       echoCommand     ),
                ( "hexdump",    hexdumpCommand  ),
                ( "ls",         lsCommand       ),
                ( "mkdir",      mkdirCommand    ),
                ( "mv",         mvCommand       ),
                ( "mvdir",      mvdirCommand    ),
                ( "pwd",        pwdCommand      ),
                ( "rm",         rmCommand       ),
                ( "rmdir",      rmdirCommand    )
            ]



-- Create a new, blank environment, from the current directory
blankEnvironment :: IO Environment
blankEnvironment = do
    fp <- getCurrentDirectory
    return $ Environment {
        commandList = defaultCommands,
        currentDirectory = fp,
        variables = M.empty }


-- Create a new, blank environment, with a custom directory
blankDirEnvironment :: FilePath -> IO Environment
blankDirEnvironment fp = do
    env <- blankEnvironment
    return $ env { currentDirectory = fp }


