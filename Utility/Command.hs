module Utility.Command (
    getArg,
    getCommand,
    isSpecialArg
) where

import Data.Maybe
import qualified Data.Map as M

import Language.Definitions

-- unnecessary
getArg :: [ PrimitiveType ] -> Int -> Maybe PrimitiveType
getArg xs n
    | length xs <= n    = Nothing
    | otherwise         = Just ( xs !! n )

-- unnecessary
getCommand :: Environment -> String -> CommandFunction
getCommand env name = commandList env M.! name


isSpecialArg :: String -> Bool
isSpecialArg [] = False
isSpecialArg ( x : xs ) = x == '-'


