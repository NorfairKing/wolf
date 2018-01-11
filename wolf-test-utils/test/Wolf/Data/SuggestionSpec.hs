{-# LANGUAGE TypeApplications #-}

module Wolf.Data.SuggestionSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Suggestion
import Wolf.Data.TestUtils

import Wolf.Data.Gen ()

spec :: Spec
spec = do
    describe "hashSuggestion" $
        it "hashes the same entry to the same hash" $
        producesValidsOnValids (hashSuggestion @Int)
    withDataSetsGen $ do
        describe "readPersonEntrySuggestions" $ do
            it "reads the suggestions that were just written" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \sugs -> do
                        sugs' <-
                            flip runReaderT sets $ do
                                writeUnusedSuggestions entrySuggestionType sugs
                                readUnusedSuggestions entrySuggestionType
                        sugs' `shouldBe` sugs
            it "reads the suggestions that were just added" $ \gen ->
                once $
                forAll gen $ \sets ->
                    forAllValid $ \sugs -> do
                        sugs' <-
                            flip runReaderT sets $ do
                                addUnusedSuggestions entrySuggestionType sugs
                                readUnusedSuggestions entrySuggestionType
                        sugs' `shouldBe` sugs
        describe "readUsedPersonEntrySuggestions" $ do
            it "reads the suggestions that were just written" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \sugs -> do
                        sugs' <-
                            flip runReaderT sets $ do
                                writeUsedSuggestions entrySuggestionType sugs
                                readUsedSuggestions entrySuggestionType
                        sugs' `shouldBe` sugs
            it "reads the suggestions that were just added" $ \gen ->
                once $
                forAll gen $ \sets ->
                    forAllValid $ \sugs -> do
                        sugs' <-
                            flip runReaderT sets $ do
                                recordUsedSuggestions entrySuggestionType sugs
                                readUsedSuggestions entrySuggestionType
                        sugs' `shouldBe` sugs
