{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Init.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data

import Wolf.Data.Gen ()

spec :: Spec
spec = do
    eqSpec @InitData
    genValidSpec @InitData
    jsonSpecOnValid @InitData
