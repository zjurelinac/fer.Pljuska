module Utility.Data (
    prettyBytes,
    splitOn,
    toInt,
    toString,
    wordToHex
) where

import Language.Definitions

import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Word
import Text.Printf


-- Convert a PrimitiveType to a String
toString :: PrimitiveType -> String
toString ( IntValue i )     = show i
toString ( StringValue s )  = s
toString NoValue            = ""


toInt :: PrimitiveType -> Int
toInt ( IntValue i )        = i
toInt ( StringValue s )     = read s :: Int
toInt NoValue               = error "Cannot convert to int"


splitOn :: String -> String -> [ String ]
splitOn delims = reverse . map reverse . foldl splitter []
    where
        splitter :: [ String ] -> Char -> [ String ]
        splitter [] y = if y `elem` delims then [ "" ] else [ [ y ] ]
        splitter xl@( x : xs ) y
            | y `elem` delims   = "" : xl
            | otherwise         = ( y : x ) : xs


prettyBytes :: BL.ByteString -> String
prettyBytes = intercalate " " . map wordToHex . BL.unpack


wordToHex :: Word8 -> String
wordToHex = printf "%02X"
