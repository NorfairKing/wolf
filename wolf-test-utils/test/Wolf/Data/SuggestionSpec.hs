module Wolf.Data.SuggestionSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Suggestion
import Wolf.Data.TestUtils

import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.Suggestion.Types.Gen ()

spec :: Spec
spec =
    withDataSetsGen $ do
        describe "readPersonEntrySuggestions" $
            it "reads the suggestions that were just written" $ \gen ->
                forAll gen $ \sets ->
                    forAll genValid $ \sugs -> do
                        sugs' <-
                            flip runReaderT sets $ do
                                writePersonEntrySuggestions sugs
                                readPersonEntrySuggestions
                        sugs' `shouldBe` sugs
        describe "readUsedPersonEntrySuggestions" $
            it "reads the suggestions that were just written" $ \gen ->
                forAll gen $ \sets ->
                    forAll genValid $ \sugs -> do
                        sugs' <-
                            flip runReaderT sets $ do
                                writeUsedPersonEntrySuggestions sugs
                                readUsedPersonEntrySuggestions
                        sugs' `shouldBe` sugs
