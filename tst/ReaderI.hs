{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ReaderI where

import Test.Framework
import Test.HUnit.Base
import Text.ParserCombinators.Parsec as P

import Data.Ini.Reader.Internals

-- Convenience function that translates a parser result to something that's
-- easier to check.
p2E p s t = let
        res = P.parse p s t
    in case res of
        Left _ -> Left "bad"
        Right e -> Right e

-- {{{1 secParser
test_secParserBasic = let
        expected = Right $ SectionL "foo"
        actual = p2E secParser "sec" "[foo]\n"
    in assertEqual expected actual

test_secParserDropSpace = let
        expected = Right $ SectionL "foo"
        actual = p2E secParser "sec" "[ \tfoo\t ]\n"
    in assertEqual expected actual

test_secParserDropTrailing = let
        expected = Right $ SectionL "foo"
        actual = p2E secParser "sec" "[foo]  \t foobar\n"
    in assertEqual expected actual

-- {{{1 optLineParser
test_optLineParserBasic = let
        expected = Right $ OptionL "foo" "bar"
        actual = p2E optLineParser "optLine" "foo=bar\n"
    in assertEqual expected actual

test_optLineParserDropSpace = let
        expected = Right $ OptionL "foo" "bar"
        actual = p2E optLineParser "optLine" "foo\t \t=\t \t bar\n"
    in assertEqual expected actual

test_optLineParserKeepSpace = let
        expected = Right $ OptionL "foo" "bar \t \t"
        actual = p2E optLineParser "optLine" "foo\t \t=\t \t bar \t \t\n"
    in assertEqual expected actual

-- {{{1 optContParser
test_optContParserSpace = let
        expected = Right $ OptionContL "foo"
        actual = p2E optContParser "optCont" " foo\n"
    in assertEqual expected actual

test_optContParserTab = let
        expected = Right $OptionContL "foo"
        actual = p2E optContParser "optCont" "\tfoo\n"
    in assertEqual expected actual

test_optContParserKeepTrailing = let
        expected = Right $ OptionContL "foo  \t\t"
        actual = p2E optContParser "optCont" "\tfoo  \t\t\n"
    in assertEqual expected actual

-- {{{1 noiseParser
test_noiseParserEmptyLine = let
        expected = Right CommentL
        actual = p2E noiseParser "noise" "\n"
    in assertEqual expected actual

test_noiseParserComment = let
        expected = Right CommentL
        actual = p2E noiseParser "noise" "# a comment\n"
    in assertEqual expected actual

test_noiseParserNonEmpty = let
        expected = Left "bad"
        actual = p2E noiseParser "noise" " \n"
    in assertEqual expected actual

-- {{{1 iniParser
-- TBD

-- {{{1 buildConfig
-- TBD
