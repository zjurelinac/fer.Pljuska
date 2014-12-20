module Language.Commands.Basic (
    catCommand,
    cdCommand,
    echoCommand,
    hexdumpCommand,
    lsCommand,
    pwdCommand
) where

import Data.List
import qualified Data.ByteString.Lazy as BL
import System.Directory

import Language.Definitions
import Utility.Data
import Utility.File


catCommand :: CommandFunction
catCommand env args = do
    let fn = toString $ head args
    x <- readFile fn
    return $ ( StringValue x, env )


cdCommand :: CommandFunction
cdCommand env args
    | length args == 0      = do
        let f = getHomeDirectory
        setCurrentDirectory f
        return ( defaultReturn, env { currentDirectory = f } )
    | otherwise             = do
        let f = getAbsolutePath ( currentDirectory env ) ( toString $ head args )
        setCurrentDirectory f
        return ( defaultReturn, env { currentDirectory = f } )


echoCommand :: CommandFunction
echoCommand env args = return ( StringValue $ concatMap toString args, env )


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
    fs <- getDirectoryContents f
    return $ ( StringValue $ intercalate "\n" ( sort fs ), env )


pwdCommand :: CommandFunction
pwdCommand env _ = return ( StringValue $ currentDirectory env, env )


