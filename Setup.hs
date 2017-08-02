#! /usr/bin/env runhaskell
-- Copyright : 2011-2017 Magnus Therning
-- License   : BSD3
module Main where

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.Simple.LocalBuildInfo
import Control.Monad
import System.FilePath
import System.Directory
import System.IO.Error

main = defaultMainWithHooks $ simpleUserHooks
    { cleanHook = profileClean
    }

profileClean pd v uh cf = let
        _matchFileGlob g = catchIOError (matchFileGlob g) (\ _ -> return [])
    in do
        (cleanHook simpleUserHooks) pd v uh cf
        tixFiles <- _matchFileGlob "*.tix"
        mapM_ removeFile tixFiles
        doesDirectoryExist ".hpc" >>= \ d -> when d $ removeDirectoryRecursive ".hpc"
