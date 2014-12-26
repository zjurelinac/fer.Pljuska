module Utility.Data (
    isIdentifier,
    isIdentifierStart,
    isVarIdentifier,
    prettyBytes,
    splitOn,
    toInt,
    toString,
    trimBefore,
    wordToHex
) where

import Language.Definitions

import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.List
import Data.Word
import Text.Printf


-- Primitive type conversions

toString :: PrimitiveType -> String
toString ( IntValue i )     = show i
toString ( StringValue s )  = s
toString NoValue            = ""


toInt :: PrimitiveType -> Int
toInt ( IntValue i )        = i
toInt ( StringValue s )     = read s :: Int
toInt NoValue               = error "Cannot convert to int"



-- String utility functions

splitOn :: String -> String -> [ String ]
splitOn delims = reverse . map reverse . foldl splitter []
    where
        splitter :: [ String ] -> Char -> [ String ]
        splitter [] y = if y `elem` delims then [ "" ] else [ [ y ] ]
        splitter xl@( x : xs ) y
            | y `elem` delims   = "" : xl
            | otherwise         = ( y : x ) : xs


trimBefore :: String -> String
trimBefore = dropWhile isSpace




-- Hex data functions

prettyBytes :: BL.ByteString -> String
prettyBytes = intercalate " " . map wordToHex . BL.unpack


wordToHex :: Word8 -> String
wordToHex = printf "%02X"



-- Token selection functions

isVarIdentifier :: Char -> Bool
isVarIdentifier x = isAlphaNum x || x == '_'

isIdentifierStart :: Char -> Bool
isIdentifierStart x = isAlpha x || x `elem` "/_.-"

isIdentifier :: Char -> Bool
isIdentifier x = isAlphaNum x || x `elem` "/_:\\.-="
