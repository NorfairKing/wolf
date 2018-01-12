{-# LANGUAGE TypeApplications #-}

module Wolf.Data.SuggestionSpec
    ( spec
    ) where

import TestImport

import qualified Data.Map as M
import qualified Data.Set as S

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
                    forAllValid $ \typ ->
                        forAllValid $ \sugs -> do
                            sugs' <-
                                flip runReaderT sets $ do
                                    writeUnusedSuggestions @EntrySuggestion typ sugs
                                    readUnusedSuggestions typ
                            S.fromList (M.elems sugs') `shouldBe` sugs
            it "reads the suggestions that were just added" $ \gen ->
                once $
                forAll gen $ \sets ->
                    forAllValid $ \typ ->
                        forAllValid $ \sugs -> do
                            sugs' <-
                                flip runReaderT sets $ do
                                    addUnusedSuggestions @EntrySuggestion typ sugs
                                    readUnusedSuggestions typ
                            S.fromList (M.elems sugs') `shouldBe` sugs
        describe "readUsedPersonEntrySuggestions" $ do
            it "reads the suggestions that were just written" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \typ ->
                        forAllValid $ \sugs -> do
                            sugs' <-
                                flip runReaderT sets $ do
                                    writeUsedSuggestions @EntrySuggestion typ sugs
                                    readUsedSuggestions typ
                            S.fromList (M.elems sugs') `shouldBe` sugs
            it "reads the suggestions that were just added" $ \gen ->
                once $
                forAll gen $ \sets ->
                    forAllValid $ \typ ->
                        forAllValid $ \sugs -> do
                            sugs' <-
                                flip runReaderT sets $ do
                                    recordUsedSuggestions @EntrySuggestion typ sugs
                                    readUsedSuggestions typ
                            S.fromList (M.elems sugs') `shouldBe` sugs
