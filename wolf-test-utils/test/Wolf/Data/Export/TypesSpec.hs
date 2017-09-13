{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Export.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Export.Types

import Wolf.Data.Gen ()

spec :: Spec
spec = do
    eqSpec @Export
    genValidSpec @Export
    jsonSpecOnValid @Export
