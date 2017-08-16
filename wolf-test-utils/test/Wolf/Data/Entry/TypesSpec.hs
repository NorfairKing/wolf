{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Entry.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Entry.Types

import Wolf.Data.Entry.Types.Gen ()

spec :: Spec
spec = do
    jsonSpecOnValid @PersonEntry
    jsonSpecOnValid @PersonPropertyValue
