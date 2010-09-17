module Data.Ini.Types where

import qualified Data.Map as M

type Config = M.Map SectionName Section

type SectionName = String
type Section = M.Map OptionName OptionValue

type OptionName = String
type OptionValue = String

-- useful since Map doesn't have any Serial instance
cfgFromList :: [(SectionName, [(OptionName, OptionValue)])] -> Config
cfgFromList = M.fromList . map (\ (sn, ol) -> (sn, M.fromList ol))

cfgToList :: Config -> [(SectionName, [(OptionName, OptionValue)])]
cfgToList = M.toList . M.map M.toList
