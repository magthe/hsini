{-# OPTIONS_GHC -XTemplateHaskell #-}
-- Copyright : 2011 Magnus Therning
-- License   : BSD3
module Ini where

-- {{{1 imports
import Data.List
import Data.Maybe
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.Ini
import Data.Ini.Types

-- {{{1 section properties
-- adding and then deleting a section is a no-op (if the section doesn't exist
-- already)
prop_secAddDel sn cfglst = delSection sn (setOption sn "foo" "bar" cfg2) == cfg2
    where
        cfg = cfgFromList cfglst
        cfg2 = delSection sn cfg -- must make sure the section doesn't exist before adding

-- after adding a section the config has such a section
prop_secAddHas sn cfglst = hasSection sn (setOption sn "foo" "bar" cfg)
    where cfg = cfgFromList cfglst

-- after adding a section it's possible to get it
prop_secAddGet sn cfglst = isJust $ getSection sn (setOption sn "foo" "bar" cfg)
    where cfg = cfgFromList cfglst

-- after deleting a section it's gone
prop_secDelGet sn cfglst = isNothing $ getSection sn $ delSection sn cfg2
    where
        cfg = cfgFromList cfglst
        cfg2 = setOption sn "foo" "bar" cfg

-- {{{1 option properties
-- setting and then deleting an option is a no-op (if the option doesn't exist
-- already)
prop_optSetDel sn on ov cfglst = delOption sn on (setOption sn on ov cfg) == cfg2
    where
        cfg = cfgFromList cfglst
        cfg2 = delOption sn on cfg

-- after setting an option it's there
prop_optSetHas sn on ov cfglst = hasOption sn on (setOption sn on ov cfg)
    where cfg = cfgFromList cfglst

-- after setting an option it's possible to get it
prop_optSetGet sn on ov cfglst = isJust $ getOption sn on $ setOption sn on ov cfg
    where cfg = cfgFromList cfglst

-- after deleting a section it's gone
prop_optDelGet sn on ov cfglst = isNothing $ getOption sn on $ delOption sn on cfg2
    where
        cfg = cfgFromList cfglst
        cfg2 = setOption sn on ov cfg

-- getting all items
prop_optAllItems cfglst = (length _cfglst > 0) ==> lstItems == (allItems sn cfg)
    where
        cfg = cfgFromList cfglst
        _cfglst = cfgToList cfg
        -- sn = head . sort $ map fst _cfglst
        sn = head $ map fst _cfglst
        lstItems = fromJust $ lookup sn _cfglst

-- {{{1 allTests
allTests = $(testGroupGenerator)
