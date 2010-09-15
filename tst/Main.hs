module Main where

import Test.Framework

import qualified Ini as I
import qualified ReaderI as RI

main = defaultMain
    [ I.allTests
    , RI.allTests
    ]
