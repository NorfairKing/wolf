{-# LANGUAGE TypeApplications #-}

module Wolf.Data.BakedSpec
  ( spec
  ) where

import TestImport

import Wolf.Data.Baked

import Wolf.Data.Baked.Gen ()
import Wolf.Data.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @Met
  genValidSpec @Met
  eqSpecOnValid @Gender
  genValidSpec @Gender
  eqSpecOnValid @Name
  genValidSpec @Name
