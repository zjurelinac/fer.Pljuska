module Utility.Command (
    getArg
) where

import Data.Maybe

import Language.Definitions

getArg :: [ PrimitiveType ] -> Int -> Maybe PrimitiveType
getArg xs n
    | length xs <= n    = Nothing
    | otherwise         = Just ( xs !! n )
