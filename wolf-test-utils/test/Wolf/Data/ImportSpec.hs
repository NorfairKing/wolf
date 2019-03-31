{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.ImportSpec
  ( spec
  ) where

import TestImport

import Wolf.Data
import Wolf.Data.Gen ()

import Wolf.Data.TestUtils

spec :: Spec
spec =
  withDataSetsGen $
  describe "importRepo" $
  it "imports a valid Repo to make a valid repository" $ \gen ->
    forAll gen $ \sets ->
      forAllValid $ \repo -> do
        e <-
          flip runReaderT sets $ do
            ensureClearRepository
            importRepo repo
        e `shouldSatisfy` isValid
