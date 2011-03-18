#! /usr/bin/env runhaskell
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- Copyright : 2011 Magnus Therning
-- License   : BSD3
module Main where

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.Simple.LocalBuildInfo
import System.Cmd
import Control.Monad
import System.FilePath
import System.Directory

main = defaultMainWithHooks $ simpleUserHooks
    { cleanHook = profileClean
    , runTests = runTestsBuild
    }

profileClean pd v uh cf = let
        _matchFileGlob g = catch (matchFileGlob g) (\ _ -> return [])
    in do
        (cleanHook simpleUserHooks) pd v uh cf
        tixFiles <- _matchFileGlob "*.tix"
        mapM_ removeFile tixFiles
        doesDirectoryExist ".hpc" >>= \ d -> when d $ removeDirectoryRecursive ".hpc"

runTestsBuild a b pd lbi = let
        doWithExe bldDir e _ = let
                _eN = exeName e
                _exe = bldDir </> _eN </> _eN
                _runTest = do
                    putStrLn $ "** " ++ _eN ++ ":"
                    system _exe >> return ()
            in
                when (_eN `elem` ["tests"]) _runTest
    in do
        (runTests simpleUserHooks) a b pd lbi
        withExeLBI pd lbi (doWithExe $ buildDir lbi)
