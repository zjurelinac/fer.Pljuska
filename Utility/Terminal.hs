module Utility.Terminal (
    Color (..),
    Style (..),
    Weight (..),
    errorString,
    infoString,
    styledString
) where


data Color = Red | Green | Yellow | Blue | Magenta | Cyan | Black | White | Default deriving ( Show )
data Weight = Normal | Bold deriving ( Show )

data Style = Style Color Weight deriving ( Show )


startSeq :: String
startSeq = "\x1B["


resetCode :: String
resetCode = startSeq ++ "0m"


colorCode :: Color -> String
colorCode Red       = "31"
colorCode Green     = "32"
colorCode Yellow    = "33"
colorCode Blue      = "34"
colorCode Magenta   = "35"
colorCode Cyan      = "36"
colorCode Black     = "48"
colorCode White     = "37"
colorCode Default   = "48"


weightCode :: Weight -> String
weightCode Normal   = ""
weightCode Bold     = ";1"


styleSeq :: Style -> String
styleSeq ( Style c w ) = startSeq ++ colorCode c ++ weightCode w ++ "m"


styledString :: Style -> String -> String
styledString s str = styleSeq s ++ str ++ resetCode


errorString :: String -> String
errorString = styledString ( Style Red Bold )


infoString :: String -> String
infoString = styledString ( Style Blue Normal )
