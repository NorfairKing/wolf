module Wolf.Data.IndexSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Index

import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.TestUtils
import Wolf.Data.Types.Gen ()

spec :: Spec
spec =
    withDataSetsGen $ do
        describe "getPersonEntry" $
            it "gets the entry that was just put" $ \gen ->
                forAll gen $ \dataSets ->
                    forAll genValid $ \(personUuid, personEntry) -> do
                        mpe <-
                            flip runReaderT dataSets $ do
                                putPersonEntry personUuid personEntry
                                getPersonEntry personUuid
                        mpe `shouldBe` Just personEntry
        describe "getIndex" $
            it "gets the index that was just put" $ \gen ->
                forAll gen $ \dataSets ->
                    forAll genValid $ \index -> do
                        index' <-
                            flip runReaderT dataSets $ do
                                putIndex index
                                getIndex
                        index' `shouldBe` Just index
