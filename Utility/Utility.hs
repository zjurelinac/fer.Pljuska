import qualified System.Console.ANSI as Term

introMessage :: String
introMessage = "Welcome to Pljuska, a Hash scripting language interpreter,\nwritten completely in Haskell for PUH Course 2014/2015.\n"

helpMessage :: String
helpMessage = "For help and a list of commands, type 'help'.\n"


displayStartScreen :: IO ()
displayStartScreen = do
    Term.setSGR [ Term.SetColor Term.Foreground Term.Dull Term.Cyan ]
    putStrLn introMessage
    Term.setSGR [ Term.SetColor Term.Foreground Term.Dull Term.Green ]
    putStrLn helpMessage
    Term.setSGR[ Term.Reset ]


displayPrompt :: IO ()
displayPrompt = do
    Term.setSGR [ Term.SetColor Term.Foreground Term.Dull Term.White,
                  Term.SetConsoleIntensity Term.BoldIntensity ]
    putStr "λ>"
    Term.setSGR[ Term.Reset ]


setTermTitle :: IO ()
setTermTitle = Term.setTitle "Pljuska v1.0.0"


