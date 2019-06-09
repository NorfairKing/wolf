{-# LANGUAGE TypeApplications #-}

module Wolf.Data.People.TypesSpec
  ( spec
  ) where

import TestImport

import Wolf.Data

import Wolf.Data.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @PersonUuid
  genValidSpec @PersonUuid
  jsonSpec @PersonUuid
