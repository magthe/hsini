{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module    : Data.Ini
Copyright : 2011-2014 Magnus Therning
License   : BSD3

A representation of configuration options.  It consists of /sections/,
each which can contain 0 or more /options/.  Each options is a /key/,
/value/ pair.

This module contains the API for constructing, manipulating, and querying
configurations.
-}
module Data.Ini where

-- {{{1 imports
import Data.Map qualified as M
import Data.Maybe (isJust)

import Data.Ini.Types (ConfigT, SectionT)

-- {{{1 configurations

-- | Constructs an empty configuration.
emptyConfig :: ConfigT a
emptyConfig = M.empty

-- {{{1 sections

-- | Returns @True@ iff the configuration has a section with that name.
hasSection :: Ord a => a -> ConfigT a -> Bool
hasSection = M.member

-- | Returns the section with the given name if it exists in the configuration.
getSection :: Ord a => a -> ConfigT a -> Maybe (SectionT a)
getSection = M.lookup

-- | Returns a list of the names of all section.
sections :: ConfigT a -> [a]
sections = M.keys

-- | Removes the section if it exists.
delSection :: Ord a => a -> ConfigT a -> ConfigT a
delSection = M.delete

-- {{{1 options

-- | Returns @True@ if the names section has the option.
hasOption :: Ord a => a -> a -> ConfigT a -> Bool
hasOption sn on cfg = isJust $ getSection sn cfg >>= M.lookup on

-- | Returns the value of the option, if it exists.
getOption :: Ord a => a -> a -> ConfigT a -> Maybe a
getOption sn on cfg = getSection sn cfg >>= M.lookup on

-- | Returns a list of all options in the section.
options :: Ord a => a -> ConfigT a -> [a]
options sn cfg = maybe [] M.keys (getSection sn cfg)

-- | Sets the value of the option, adding it if it doesn't exist.
setOption :: Ord a => a -> a -> a -> ConfigT a -> ConfigT a
setOption sn on ov cfg = maybe (M.insert sn new_s cfg) (\sec -> M.insert sn (M.insert on ov sec) cfg) s
  where
    s = getSection sn cfg
    new_s = M.insert on ov M.empty

-- | Removes the option if it exists.  Empty sections are pruned.
delOption :: Ord a => a -> a -> ConfigT a -> ConfigT a
delOption sn on cfg =
    if sEmptyAfterDelete
        then M.delete sn cfg
        else maybe cfg (\sec -> M.insert sn (M.delete on sec) cfg) s
  where
    s = getSection sn cfg
    sEmptyAfterDelete = maybe True (\sec -> M.empty == M.delete on sec) s

-- | Returns all options and their values of a section.
allItems :: Ord a => a -> ConfigT a -> [(a, a)]
allItems sn cfg = maybe [] M.toList (getSection sn cfg)
