{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module    : Data.Ini.Types
Copyright : 2011-2014 Magnus Therning
License   : BSD3
-}
module Data.Ini.Types where

import Control.Arrow (second)
import Data.Map qualified as M

type ConfigT a = M.Map a (SectionT a)
type SectionT a = M.Map a a

type Config = ConfigT String

-- useful since Map doesn't have any Serial instance
cfgFromList :: Ord a => [(a, [(a, a)])] -> ConfigT a
cfgFromList = M.fromList . map (second M.fromList)

cfgToList :: ConfigT a -> [(a, [(a, a)])]
cfgToList = M.toList . M.map M.toList
