module Utility.Data (
    prettyBytes,
    wordToHex
) where

import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Word
import Text.Printf


prettyBytes :: BL.ByteString -> String
prettyBytes = intercalate " " . map wordToHex . BL.unpack


wordToHex :: Word8 -> String
wordToHex = printf "%02X"
