module Language.Commands.Basic (
    catCommand,
    cdCommand,
    echoCommand,
    exitCommand,
    hexdumpCommand,
    lsCommand,
    panicCommand,
    pwdCommand
) where

import Data.List
import qualified Data.ByteString.Lazy as BL
import System.Directory
import System.Exit
import System.Path

import Language.Definitions
import Utility.Command
import Utility.Data
import Utility.File
import Utility.Terminal


catCommand :: CommandFunction
catCommand env args = do
    let fn = toString $ head args
    x <- readFile fn
    return $ ( StringValue x, env )


cdCommand :: CommandFunction
cdCommand env args
    | length args == 0      = do
        f <- getHomeDirectory
        setCurrentDirectory f
        return ( NoValue, env { currentDirectory = f } )
    | otherwise             = do
        let f = getAbsolutePath ( currentDirectory env ) ( toString $ head args )
        setCurrentDirectory f
        return ( NoValue, env { currentDirectory = f } )


echoCommand :: CommandFunction
echoCommand env args = return ( StringValue $ concatMap toString args, env )


exitCommand :: CommandFunction
exitCommand env args = exitSuccess


grepCommand :: CommandFunction
grepCommand env args
    | null args     = error "Wrong grep command call"
    | isCount       = ( IntVal $ length ls, env )
    | otherwise     = ( NoValue, env )

    where
            args'       =   map toString args
            isCount     =   hasArgument "-c" args'
            noCase      =   hasArgument "-i" args'
            showNumbers =   hasArgument "-n" args'
            invert      =   hasArgument "-v" args'
            onlyPart    =   hasArgument "-o" args'

            hasArgument x = not . null . length . filter ( == x )

            transform   =   if noCase then lower else id
            modifier    =   if invert then not else id

            pattern     =   tail . init $ args'


hexdumpCommand :: CommandFunction
hexdumpCommand env args = do
    x <- BL.readFile $ toString $ head args
    return ( StringValue $ prettyBytes x, env )


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
        h <- readFile "Data/Help/main.txt"
        return ( StringValue h, env )
-- Help for a particular command
    | otherwise     = do
        let a = args !! 0
        h <- readFile $ "Data/Help/" ++ toString a ++ ".txt"
        return ( StringValue h, env )


pwdCommand :: CommandFunction
pwdCommand env _ = return ( StringValue $ currentDirectory env, env )


