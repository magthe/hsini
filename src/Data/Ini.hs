-- |
-- Module    : Data.Ini
-- Copyright : 2011 Magnus Therning
-- License   : BSD3
--
-- A representation of configuration options.  It consists of /sections/,
-- each which can contain 0 or more /options/.  Each options is a /key/,
-- /value/ pair.
--
-- This module contains the API for constructing, manipulating, and querying
-- configurations.
module Data.Ini where

-- {{{1 imports
import qualified Data.Map as M
import Data.Maybe

import Data.Ini.Types

-- {{{1 configurations
-- | Constructs an empty configuration.
emptyConfig :: Config
emptyConfig = M.empty

-- {{{1 sections
-- | Returns @True@ iff the configuration has a section with that name.
hasSection :: SectionName -> Config -> Bool
hasSection = M.member

-- | Returns the section with the given name if it exists in the configuration.
getSection :: SectionName -> Config -> Maybe Section
getSection = M.lookup

-- | Returns a list of the names of all section.
sections :: Config -> [SectionName]
sections = M.keys

-- | Removes the section if it exists.
delSection :: SectionName -> Config -> Config
delSection = M.delete

-- {{{1 options
-- | Returns @True@ if the names section has the option.
hasOption :: SectionName -> OptionName -> Config -> Bool
hasOption sn on cfg = isJust $ M.lookup on =<< getSection sn cfg

-- | Returns the value of the option, if it exists.
getOption :: SectionName -> OptionName -> Config -> Maybe OptionValue
getOption sn on cfg = M.lookup on =<< getSection sn cfg

-- | Returns a list of all options in the section.
options ::  SectionName -> Config -> [OptionName]
options sn cfg = maybe [] (M.keys) (getSection sn cfg)

-- | Sets the value of the option, adding it if it doesn't exist.
setOption :: SectionName -> OptionName -> OptionValue -> Config -> Config
setOption sn on ov = M.insertWith M.union sn (M.singleton on ov)

-- | Removes the option if it exists.  Empty sections are pruned.
delOption :: SectionName -> OptionName -> Config -> Config
delOption sn on = M.update (clean . M.delete on) sn
  where
    clean opt | M.null opt = Nothing
              | otherwise  = Just opt

-- | Returns all options and their values of a section.
allItems :: SectionName -> Config -> [(OptionName, OptionValue)]
allItems sn cfg = maybe [] M.toList (getSection sn cfg)
