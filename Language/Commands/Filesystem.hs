module Language.Commands.Filesystem (
    createCommand,
    chmodCommand,
    cpCommand,
    cpdirCommand,
    mkdirCommand,
    mvCommand,
    mvdirCommand,
    rmCommand,
    rmdirCommand
) where

import Data.List
import System.Directory
import System.Path

import Language.Definitions
import Utility.Data
import Utility.File


mvCommand :: CommandFunction
mvCommand env args
    | length args == 2  = do
        renameFile ( toString $ head args ) ( toString $ last args )
        return ( NoValue, env )
    | otherwise         = do
        let dest = directorize . toString $ last args
        mapM_ ( \x -> renameFile x ( dest ++ x ) ) $ map toString $ init args
        return ( NoValue, env )


cpCommand :: CommandFunction
cpCommand env args
    | length args == 2  = do
        copyFile ( toString $ head args ) ( toString $ last args )
        return ( NoValue, env )
    | otherwise         = do
        let dest = directorize . toString $ last args
        mapM_ ( \x -> copyFile x ( dest ++ x ) ) $ map toString $ init args
        return ( NoValue, env )


createCommand :: CommandFunction
createCommand env args = do
    mapM_ ( flip writeFile "" ) $ map toString args
    return ( NoValue, env )


rmCommand :: CommandFunction
rmCommand env args = do
    mapM_ removeFile $ map toString args
    return ( NoValue, env )


mvdirCommand :: CommandFunction
mvdirCommand env args
    | length args == 2      = do
        renameDirectory ( toString $ head args ) ( toString $ last args )
        return ( NoValue, env )
    | otherwise             = do
        let dest = directorize . toString $ last args
        mapM_ ( \x -> renameDirectory x ( dest ++ x ) ) $ map toString $ init args
        return ( NoValue, env )


cpdirCommand :: CommandFunction
cpdirCommand env args
    | length args == 2  = do
        copyDir ( toString $ head args ) ( toString $ last args )
        return ( NoValue, env )
    | otherwise         = do
        let dest = directorize . toString $ last args
        mapM_ ( \x -> copyDir x ( dest ++ x ) ) $ map toString $ init args
        return ( NoValue, env )


mkdirCommand :: CommandFunction
mkdirCommand env args = do
    mapM_ createDirectory $ map toString args
    return ( NoValue, env )


rmdirCommand :: CommandFunction
rmdirCommand env args = do
    mapM_ removeDirectory $ map toString args
    return ( NoValue, env )


chmodCommand :: CommandFunction
chmodCommand env args
    | length args == 2  = do
        let f = toString $ last args
        let c = toString $ head args
        ops <- getPermissions f
        isf <- doesFileExist f
        nps <- return $ case head c of
            '+'     ->  addPerms ops isf $ tail c
            '-'     ->  removePerms ops isf $ tail c
            '='     ->  setPerms ops isf $ tail c
            _       ->  setPerms ops isf c
        setPermissions f nps
        return ( NoValue, env )
    | otherwise         = error "chmod needs exactly 2 arguments, a permission mask and a file"

    where
            setPerms :: Permissions -> Bool -> String -> Permissions
            setPerms ps b xs = ps { readable = 'r' `elem` xs,
                                    writable = 'w' `elem` xs,
                                    executable = 'x' `elem` xs && b,
                                    searchable = 'x' `elem` xs && not b }


            addPerms :: Permissions -> Bool -> String -> Permissions
            addPerms ps b xs = addReadPerm xs . addWritePerm xs . ( if b then addExecPerm xs else addSearchPerm xs ) $ ps

            addReadPerm xs ps
                | 'r' `elem` xs = ps { readable = True }
                | otherwise     = ps

            addWritePerm xs ps
                | 'w' `elem` xs = ps { writable = True }
                | otherwise     = ps

            addExecPerm xs ps
                | 'x' `elem` xs = ps { executable = True }
                | otherwise     = ps

            addSearchPerm xs ps
                | 'x' `elem` xs = ps { searchable = True }
                | otherwise     = ps


            removePerms :: Permissions -> Bool -> String -> Permissions
            removePerms ps b xs = removeReadPerm xs . removeWritePerm xs . ( if b then removeExecPerm xs else removeSearchPerm xs ) $ ps

            removeReadPerm xs ps
                | 'r' `elem` xs = ps { readable = False }
                | otherwise     = ps

            removeWritePerm xs ps
                | 'w' `elem` xs = ps { writable = False }
                | otherwise     = ps

            removeExecPerm xs ps
                | 'x' `elem` xs = ps { executable = False }
                | otherwise     = ps

            removeSearchPerm xs ps
                | 'x' `elem` xs = ps { searchable = False }
                | otherwise     = ps
