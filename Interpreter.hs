module Interpreter(

) where

import Data.Maybe
import qualified Data.Map as M

import Language.Definitions
import Utility.Utility


-- Function for evaluating variables
valueOf :: Data -> VarTable -> Either Error PrimitiveType
valueOf ( VarData ( Variable v ) ) vt
    | isNothing val     = Left ( CallError "Unknown value: Variable not previously defined" )
    | otherwise         = Right ( fromJust val )
    where val = M.lookup v vt
valueOf ( StaticData s ) _ = Right s
valueOf _ _ = Left ( CallError "Don't know how to evaluate a given argument" )


-- Create a new, blank environment, working directory provided
blankEnvironment :: FilePath -> Environment
blankEnvironment fp = Environment {
                        currentDirectory = fp,
                        vartable = M.empty,
                        executionStatus = StatusOK }


