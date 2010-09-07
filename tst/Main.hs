module Main where

import System.Environment ( getArgs )
import Test.Framework

import qualified Ini as I

main = do
    args <- getArgs
    runTestWithArgs args I.allHTFTests
