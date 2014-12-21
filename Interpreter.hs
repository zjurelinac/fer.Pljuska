module Interpreter where

import Data.Maybe
import qualified Data.Map as M

import System.Directory

import Language.Core
import Language.Definitions





stringParse :: Environment -> PrimitiveType -> PrimitiveType
stringParse env ( StringValue str ) = StringValue str


-- Create a new, blank environment, from the current directory
blankEnvironment :: IO Environment
blankEnvironment = do
    fp <- getCurrentDirectory
    return $ Environment {
        currentDirectory = fp,
        variables = M.empty }


-- Create a new, blank environment, with a custom directory
blankDirEnvironment :: FilePath -> IO Environment
blankDirEnvironment fp = do
    env <- blankEnvironment
    return $ env { currentDirectory = fp }


