module Interpreter where

import Data.Maybe
import qualified Data.Map as M

import System.Directory

import Language.Definitions
--import Utility.Utility


-- Function for evaluating variables
valueOf :: Data -> VarTable -> PrimitiveType
valueOf ( VarData ( Variable v ) ) vt
    | isNothing val     = error "No such variable"
    | otherwise         = fromJust val
    where val = M.lookup v vt
valueOf ( StaticData s ) _ = s
valueOf _ _ = error "Don't know how to evaluate a given argument"


stringParse :: Environment -> String -> String
stringParse env str = str


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


