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
                e <- runReaderT export sets
                e `shouldBe` Nothing
        it "only generates valid exports when a repository has been initialised" $ \gen ->
            forAll gen $ \sets ->
                forAll genValid $ \entryTups ->
                    forAll genValid $ \notes ->
                        forAll genValid $ \(sugs, usedSugs) -> do
                            e <-
                                flip runReaderT sets $ do
                                    initWolf
                                    oldIndex <- getIndexWithDefault
                                    let func ix (al, entry) = do
                                            (uuid, index) <-
                                                al `lookupOrCreateNewPerson` ix
                                            putPersonEntry uuid entry
                                            pure index
                                    newIndex <-
                                        foldM
                                            func
                                            oldIndex
                                            (entryTups :: [(Alias, PersonEntry)])
                                    putIndex newIndex
                                    mapM_ createNewNote (notes :: [Note])
                                    addPersonEntrySuggestions sugs
                                    recordUsedPersonEntrySuggestions usedSugs
                                    export
                            e `shouldSatisfy` isValid
                            e `shouldSatisfy` isJust
