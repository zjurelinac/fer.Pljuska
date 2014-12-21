module Language.Commands.Filesystem (
    createCommand,
    cpCommand,
    mkdirCommand,
    mvCommand,
    mvdirCommand,
    rmCommand,
    rmdirCommand
) where

import Data.List
import System.Directory

import Language.Core
import Language.Definitions
import Utility.File

mvCommand :: CommandFunction
mvCommand env args
    | length args == 2  = do
        renameFile ( toString $ head args ) ( toString $ last args )
        return ( defaultReturn, env )
    | otherwise         = do
        let dest = directorize . toString $ last args
        mapM_ ( \x -> renameFile x ( dest ++ x ) ) $ map toString $ init args
        return ( defaultReturn, env )


cpCommand :: CommandFunction
cpCommand env args
    | length args == 2  = do
        copyFile ( toString $ head args ) ( toString $ last args )
        return ( defaultReturn, env )
    | otherwise         = do
        let dest = directorize . toString $ last args
        mapM_ ( \x -> copyFile x ( dest ++ x ) ) $ map toString $ init args
        return ( defaultReturn, env )


createCommand :: CommandFunction
createCommand env args = do
    mapM_ ( flip writeFile "" ) $ map toString args
    return ( defaultReturn, env )


rmCommand :: CommandFunction
rmCommand env args = do
    mapM_ removeFile $ map toString args
    return ( defaultReturn, env )


mvdirCommand :: CommandFunction
mvdirCommand env args
    | length args == 2      = do
        renameDirectory ( toString $ head args ) ( toString $ last args )
        return ( defaultReturn, env )
    | otherwise             = do
        let dest = directorize . toString $ last args
        mapM_ ( \x -> renameDirectory x ( dest ++ x ) ) $ map toString $ init args
        return ( defaultReturn, env )


-- cpdirCommand :: CommandFunction
-- cpdirCommand env args
--     | length args == 2  = do
--         copyDirectory ( toString $ head args ) ( toString $ last args )
--         return ( defaultReturn, env )
--     | otherwise         = do
--         let dest = directorize . toString $ last args
--         mapM_ ( \x -> copyDirectory x ( dest ++ x ) ) $ map toString $ init args
--         return ( defaultReturn, env )


mkdirCommand :: CommandFunction
mkdirCommand env args = do
    mapM_ createDirectory $ map toString args
    return ( defaultReturn, env )


rmdirCommand :: CommandFunction
rmdirCommand env args = do
    mapM_ removeDirectory $ map toString args
    return ( defaultReturn, env )


-- chmodCommand :: CommandFunction
-- chmodCommand env args = do

