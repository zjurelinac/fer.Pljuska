module Hash(
    runInteractive,
    runScript
) where

runInteractive :: IO ()
runInteractive = print "Hash interactive"


runScript :: FilePath -> IO ()
runScript fp = do
    file <- readFile fp
    print file
