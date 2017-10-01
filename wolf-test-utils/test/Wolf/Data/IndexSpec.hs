module Wolf.Data.IndexSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Index

import Wolf.Data.Gen ()
import Wolf.Data.TestUtils

spec :: Spec
spec =
    withDataSetsGen $ do
        describe "getPersonEntry" $
            it "gets the entry that was just put" $ \gen ->
                forAll gen $ \dataSets ->
                    forAllValid $ \(personUuid, personEntry) -> do
                        mpe <-
                            flip runReaderT dataSets $ do
                                putPersonEntry personUuid personEntry
                                getPersonEntry personUuid
                        mpe `shouldBe` Just personEntry
        describe "getIndex" $
            it "gets the index that was just put" $ \gen ->
                forAll gen $ \dataSets ->
                    forAllValid $ \index -> do
                        index' <-
                            flip runReaderT dataSets $ do
                                putIndex index
                                getIndex
                        index' `shouldBe` Just index
