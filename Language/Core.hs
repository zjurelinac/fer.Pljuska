module Language.Core where

import Language.Definitions


-- Convert a PrimitiveType to a String
toString :: PrimitiveType -> String
toString ( IntValue i ) = show i
toString ( StringValue s ) = s


toInt :: PrimitiveType -> Int
toInt ( IntValue i ) = i
toInt ( StringValue s ) = read s :: Int
