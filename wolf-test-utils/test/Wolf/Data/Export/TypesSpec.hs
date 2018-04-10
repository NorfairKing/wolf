{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Export.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Export.Types

import Wolf.Data.Gen ()

spec :: Spec
spec = do
    eqSpec @Repo
    genValidSpec @Repo
    jsonSpecOnValid @Repo
    functorSpecOnValid @ResultS
    applicativeSpecOnValid @ResultS
    monadSpecOnValid @ResultS
