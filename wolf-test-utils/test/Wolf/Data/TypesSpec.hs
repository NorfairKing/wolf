{-# LANGUAGE TypeApplications #-}

module Wolf.Data.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data

import Wolf.Data.Types.Gen ()

spec :: Spec
spec = do
    eqSpec @InitData
    genValidSpec @InitData
    jsonSpecOnValid @InitData
