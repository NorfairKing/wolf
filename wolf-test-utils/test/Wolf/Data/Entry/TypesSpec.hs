{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Entry.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Entry.Types

import Wolf.Data.Entry.Types.Gen ()

spec :: Spec
spec = do
    genValidSpec @PersonEntry
    jsonSpecOnValid @PersonEntry
    genValidSpec @PersonProperty
    jsonSpecOnValid @PersonProperty
    genValidSpec @PersonPropertyValue
    jsonSpecOnValid @PersonPropertyValue
