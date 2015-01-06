import Hash

import System.Environment


main :: IO ()
main = do
    as <- getArgs
    if null as
        then runInteractive
        else runScript $ head as


