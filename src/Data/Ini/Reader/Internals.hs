-- |
-- Module    : Data.Ini.Reader.Internals
-- Copyright : 2011 Magnus Therning
-- License   : BSD3
--
-- Internal functions used in 'Data.Ini.Reader'.
module Data.Ini.Reader.Internals where

import Control.Monad.Error
import Control.Monad.State
-- import Text.ParserCombinators.Parsec as P
import Text.Parsec as P
import Text.Parsec.String
import qualified Data.ByteString as BS

import Data.Ini
import Data.Ini.Types

data IniReaderError
    = IniParserError String
    | IniSyntaxError String
    | IniOtherError String
    deriving (Eq, Show)

instance Error IniReaderError where
    noMsg = IniOtherError "Unknown error"
    strMsg s = IniOtherError s

type IniParseResult = Either IniReaderError

-- | The type used to represent a line of a config file.
data IniFile
    = SectionL String
    | OptionL String String
    | OptionContL String
    | CommentL
    deriving (Show, Eq)

-- | Build a configuration from a list of 'IniFile' items.
buildConfig :: [IniFile] -> IniParseResult Config
buildConfig ifs = let
        isComment CommentL = True
        isComment _ = False

        fIfs = filter (not . isComment) ifs

        -- merge together OptionL and subsequent OptionContL items
        mergeOptions [] = return []
        mergeOptions (s@(SectionL _) : ifs) = (s :) `liftM` mergeOptions ifs
        mergeOptions (CommentL : ifs ) = (CommentL :) `liftM` mergeOptions ifs
        mergeOptions (OptionL on ov : OptionContL ov2 : ifs) = mergeOptions $ (OptionL on (ov ++ ov2)) : ifs
        mergeOptions (o@(OptionL on ov) : ifs) = (o :) `liftM` mergeOptions ifs
        mergeOptions _ = throwError $ IniSyntaxError "Syntax error in INI file."

        -- build the configuration from a [IniFile]
        buildit a [] = return a
        buildit a (SectionL sn : is) = put sn >> buildit a is
        buildit a (OptionL on ov : is) = do
            sn <- get
            let na = setOption sn on ov a
            buildit na is

    in mergeOptions fIfs >>= (\ is -> return . fst $ runState (buildit emptyConfig is) "default")

-- | Consumer of whitespace \"@ \t@\".
eatWhiteSpace :: Parser String
eatWhiteSpace = many $ oneOf " \t"

-- | Parser for the start-of-section line.  It expects the line to start with a
-- @[@ then find the section name, and finally a @]@.  The section name may be
-- surrounded by any number of white space characters (see 'eatWhiteSpace').
secParser :: Parser IniFile
secParser = let
        validSecNameChrs = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-/@"
    in do
        char '['
        eatWhiteSpace
        sn <- many1 $ oneOf validSecNameChrs
        eatWhiteSpace
        char ']'
        manyTill anyChar newline
        return $ SectionL sn

-- | Parser for a single line of an option.  The line must start with an option
-- name, then a @=@ must be found, and finally the rest of the line is taken as
-- the option value.  The equal sign may be surrounded by any number of white
-- space characters (see 'eatWhiteSpace').
optLineParser :: Parser IniFile
optLineParser = let
        validOptNameChrs = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-/@"
    in do
        on <- many1 $ oneOf validOptNameChrs
        eatWhiteSpace
        char '='
        eatWhiteSpace
        ov <- manyTill anyChar newline
        return $ OptionL on ov

-- | Parser for an option-value continuation line.  The line must start with
-- either a space or a tab character (\"@ \t@\").  Everything else on the line,
-- until the newline character, is taken as the continuation of an option
-- value.
optContParser :: Parser IniFile
optContParser = do
    oneOf " \t"
    eatWhiteSpace
    oc <- noneOf " \t"
    ov <- manyTill anyChar newline
    return $ OptionContL $ oc:ov

-- | Parser for "noise" in the configuration file, such as comments and empty
-- lines.  (Note that lines containing only space characters will be
-- successfully parsed by 'optContParser'.)
noiseParser :: Parser IniFile
noiseParser = let
        commentP = do
            char '#'
            manyTill anyChar newline
        emptyL = newline >> return ""
    in choice [commentP, emptyL] >> return CommentL

iniParser :: Parser [IniFile]
iniParser = do
    many noiseParser
    s1 <- secParser
    r <- many $ choice [secParser, optLineParser, optContParser, noiseParser]
    return (s1:r)
