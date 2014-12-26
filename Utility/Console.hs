module Utility.Console (
    displayPrompt,
    displayStartScreen,
    setTermTitle
) where


import qualified System.Console.ANSI as Term
import System.IO

import Language.Definitions

import Utility.Terminal



introMessage :: String
introMessage = "Welcome to Pljuska, a Hash scripting language interpreter,\nwritten completely in Haskell for PUH Course 2014/2015.\n\n"


helpMessage :: String
helpMessage = "For help and a list of commands, type 'help'.\n"


displayStartScreen :: IO ()
displayStartScreen = putStrLn $ styledString ( Style Cyan Normal ) introMessage
                             ++ styledString ( Style Magenta Bold ) helpMessage


displayPrompt :: Environment -> IO ()
displayPrompt env = do
    putStr $ styledString ( Style Black Bold ) ( "λ::" ++ currentDirectory env ++ ">" )
    hFlush stdout


setTermTitle :: IO ()
setTermTitle = Term.setTitle "Pljuska v1.0.0"
