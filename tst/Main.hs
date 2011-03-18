-- Copyright : 2011 Magnus Therning
-- License   : BSD3
module Main where

import Test.Framework

import qualified Ini as I
import qualified ReaderI as RI

main = defaultMain
    [ I.allTests
    , RI.allTests
    ]
