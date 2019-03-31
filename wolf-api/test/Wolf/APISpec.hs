{-# LANGUAGE TypeApplications #-}

module Wolf.APISpec
  ( spec
  ) where

import TestImport

import Wolf.API

import Wolf.API.Gen ()

spec :: Spec
spec = do
  jsonSpecOnValid @SetPersonAlias
  jsonSpecOnValid @PersonQuery
