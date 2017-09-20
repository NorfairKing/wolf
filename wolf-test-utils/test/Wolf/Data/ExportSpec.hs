{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.ExportSpec
    ( spec
    ) where

import TestImport

import Wolf.Data
import Wolf.Data.Export.Types
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
        let roundtrip func name =
                it ("roundtrips the " ++ name) $ \gen ->
                    forAll gen $ \sets ->
                        forAllValid $ \repo -> do
                            repo' <-
                                runData sets $ do
                                    ensureClearRepository
                                    importRepo repo
                                    exportRepo
                            (func <$> repo') `shouldBe` Just (func repo)
        roundtrip repoInitData "init data"
        roundtrip repoPersonIndex "person index"
        roundtrip repoNoteIndex "note index"
        roundtrip repoNoteIndices "note indinces"
        roundtrip repoNotes "notes"
        roundtrip repoEntrySuggestions "entry suggestions"
        roundtrip repoUsedEntrySuggestions "used entry suggestions"
        roundtrip id "entire repo"
