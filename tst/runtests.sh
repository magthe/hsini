#!/bin/sh

cd "`dirname $0`"
runhaskell -i../src Main.hs $*
