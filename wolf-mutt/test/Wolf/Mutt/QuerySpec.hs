{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Mutt.QuerySpec
    ( spec
    ) where

import TestImport

import Wolf.Mutt.Query

import Wolf.Data.Gen ()
import Wolf.Mutt.Query.Gen ()

spec :: Spec
spec = do
    describe "searchResultsFor" $ do
        it "produces valid search results" $
            producesValidsOnValids2 searchResultsFor
        it "does not produce results if there is no person entry" $
            forAllValid $ \a -> searchResultsFor a Nothing `shouldBe` []
    describe "formatSearchResult" $
        it "produces valid texts" $ producesValidsOnValids formatSearchResult
    describe "formatSearchResults" $
        it "produces valid texts" $ producesValidsOnValids formatSearchResults
