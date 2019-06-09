{-# LANGUAGE TypeApplications #-}

module Wolf.Data.TypesSpec
  ( spec
  ) where

import TestImport

import Wolf.Data

import Wolf.Data.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @DataSettings
  genValidSpec @DataSettings
  jsonSpecOnValid @DataSettings
