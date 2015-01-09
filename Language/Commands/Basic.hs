module Language.Commands.Basic (
    catCommand,
    cdCommand,
    echoCommand,
    exitCommand,
    grepCommand,
    hexdumpCommand,
    lsCommand,
    panicCommand,
    pwdCommand
) where

import Data.Char
import Data.List
import System.Directory
import System.Exit
import System.Path

import Language.Definitions
import Language.Core
import Utility.Command
import Utility.Data
import Utility.File
import Utility.Terminal


catCommand :: CommandFunction
catCommand env args = do
    let fs = map toString $ args
    xs <- mapM readFile fs
    return $ ( StringValue $ intercalate "\n" xs, env )


cdCommand :: CommandFunction
cdCommand env args
    | length args == 0      = do
        f <- getHomeDirectory
        setCurrentDirectory f
        return ( NoValue, env { currentDirectory = f } )
    | otherwise             = do
        let a = ( toString $ head args )
        setCurrentDirectory a
        f <- getCurrentDirectory
        return ( NoValue, env { currentDirectory = f } )


echoCommand :: CommandFunction
echoCommand env args = return ( StringValue $ concatMap toString args, env )


exitCommand :: CommandFunction
exitCommand env args = exitSuccess


grepCommand :: CommandFunction
grepCommand env args
    | null args     = error "Wrong grep command call"
    | isCount       = return ( IntValue $ length ls, env )
    | onlyPart      = return ( StringValue $ intercalate "\n" $ replicate ( length ls ) pattern, env )
    | showNumbers   = return ( StringValue . intercalate "\n" . map ( \( x, y ) -> show x ++ " " ++ y ) $ nls, env )
    | otherwise     = return ( StringValue $ intercalate "\n" ls, env )

    where
            args'       =   map toString args
            isCount     =   containsArgument "-c" args'
            showNumbers =   containsArgument "-n" args'
            onlyPart    =   containsArgument "-o" args'

            noCase      =   containsArgument "-i" args'
            invert      =   containsArgument "-v" args'

            transform   =   map ( if noCase then toLower else id )
            modifier    =   if invert then not else id

            ls          =   filter satisfies . lines . last $ args'
            nls         =   filter ( satisfies . snd ) . zip [1..] . lines . last $ args'

            pattern     =   last . init $ args'

            satisfies :: String -> Bool
            satisfies x =   modifier $ isInfixOf ( transform pattern ) ( transform x )


hexdumpCommand :: CommandFunction
hexdumpCommand env args = do
    x <- readBinaryFile $ toString $ head args
    return ( StringValue $ x, env )


-- possibly other improvements, like styling, display options...
lsCommand :: CommandFunction
lsCommand env args = do
    let f = if( length args > 0 ) then
                toString $ head args
            else
                currentDirectory env
    d <- createDir f
    let fs = files d
    let ds = subDirs d
    return ( StringValue $ (
        folderStylize ( intercalate "\n" ( sort $ map getBaseName ds ) )
        ++ ( if null ds then "" else "\n" ) ++
        ( intercalate "\n" $ sort $ map getBaseName fs ) ), env )

    where folderStylize = styledString ( Style Blue Normal )


-- add some styling
panicCommand :: CommandFunction
panicCommand env args
-- A basic list of all commands
    | null args     = do
        h <- readFile $ toString ( evaluate env ( VarData $ Variable "BASEPATH" ) ) ++ "/Data/Help/main.txt"
        return ( StringValue h, env )
-- Help for a particular command
    | otherwise     = do
        let a = args !! 0
        h <- readFile $ "Data/Help/" ++ toString a ++ ".txt"
        return ( StringValue h, env )


pwdCommand :: CommandFunction
pwdCommand env _ = return ( StringValue $ currentDirectory env, env )


