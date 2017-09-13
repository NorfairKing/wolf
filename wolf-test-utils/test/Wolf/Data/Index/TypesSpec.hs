{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Index.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data

import Wolf.Data.Gen ()

spec :: Spec
spec = do
    eqSpec @Index
    genValidSpec @Index
    jsonSpecOnValid @Index
