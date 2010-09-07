module Main where

import System.Environment ( getArgs )
import Test.Framework

import qualified Ini as I
import qualified ReaderI as RI

main = do
    args <- getArgs
    -- runTestWithArgs args [I.allHTFTests, RI.allHTFTests]
    runTestWithArgs args [RI.allHTFTests]
