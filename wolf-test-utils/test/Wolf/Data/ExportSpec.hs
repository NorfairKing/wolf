{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.ExportSpec
    ( spec
    ) where

import TestImport

import Wolf.Data
import Wolf.Data.Gen ()

import Wolf.Data.TestUtils

spec :: Spec
spec =
    withDataSetsGen $
    describe "export" $ do
        it "exports 'Nothing' if no wolf repository has been initialised" $ \gen ->
            forAll gen $ \sets -> do
                e <-
                    flip runReaderT sets $ do
                        ensureClearRepository
                        exportRepo
                e `shouldBe` Nothing
        it "only generates valid exports when a repository has been initialised" $ \gen ->
            forAll gen $ \sets -> do
                forAll genValid $ \export -> do
                    runData sets $ setupRepo export
                    assertRepoValid sets
