-- |
-- Module    : Data.Ini.Reader
-- Copyright : 2011-2014 Magnus Therning
-- License   : BSD3
--
-- Parser for configurations.
module Data.Ini.Reader
    ( parse
    , IniReaderError(..)
    , IniParseResult
    ) where

import Control.Monad.Error
import qualified Text.ParserCombinators.Parsec as P

import Data.Ini.Types
import Data.Ini.Reader.Internals

-- | Parser for a configuration contained in a 'String'.
parse :: String -> IniParseResult Config
parse s = let
        pr = P.parse iniParser "ini" s
    in case pr of
        Left e -> throwError . IniParserError $ show e
        Right is -> buildConfig is
