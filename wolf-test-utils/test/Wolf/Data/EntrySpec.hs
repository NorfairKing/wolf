module Wolf.Data.EntrySpec
    ( spec
    ) where

import TestImport

import qualified Data.Text.Encoding as TE

import Wolf.Data.Entry

import Wolf.Data.Gen ()

spec :: Spec
spec = do
    describe "personEntry" $
        it "only generates valid PersonEntry's" $ producesValid personEntry
    describe "personEntryProperties" $
        it "only generates valid properties" $
        producesValidsOnValids personEntryProperties
    describe "newPersonEntry" $ it "is a valid entry" $ isValid newPersonEntry
    describe "entryContents" $
        it "produces valid text for any entry" $
        producesValidsOnValids entryContents
    describe "entryContentsBS" $ do
        it "produces valid bytestring for any entry" $
            producesValid entryContentsBS
        it "always produces bytestrings in UTF8 encoding" $
            forAllValid $ \pe ->
                case TE.decodeUtf8' $ entryContentsBS pe of
                    Left err ->
                        expectationFailure $
                        "Error: Unable to transform entry to text, decoding ByteString to Text failed: " <>
                        show err
                    Right r -> r `shouldSatisfy` isValid
    describe "updatePersonEntry" $ do
        it "Produces a valid result" $ producesValidsOnValids3 updatePersonEntry
        it "doesn't change anything if nothing changed" $
            forAllValid $ \pe ->
                forAllValid $ \now ->
                    case updatePersonEntry now pe (entryContentsBS pe) of
                        UpdateUnchanged -> pure ()
                        r ->
                            expectationFailure $
                            "Should have been UpdateUnchanged, was: " ++ show r
        it "does not result in UpdateUnchanged if anything changed" $
            forAllValid $ \now ->
                forAllValid $ \peOld ->
                    forAll (genValid `suchThat` (not . sameProperties peOld)) $ \peNew ->
                        case updatePersonEntry now peOld (entryContentsBS peNew) of
                            UpdateUnchanged ->
                                expectationFailure
                                    "Should not have been UpdateUnchanged."
                            _ -> pure ()
        it "keeps all the new keys in the right order" $
            forAllValid $ \now ->
                forAllValid $ \peOld ->
                    forAllValid $ \ls2 ->
                        case personEntry (PMap ls2) of
                            Nothing -> pure () -- Fine.
                            Just peNew ->
                                case updatePersonEntry
                                         now
                                         peOld
                                         (entryContentsBS peNew) of
                                    UpdateSuccess pe' ->
                                        case personEntryProperties pe' of
                                            PMap ls2' ->
                                                map fst ls2' `shouldBe`
                                                map fst ls2
                                            _ ->
                                                expectationFailure
                                                    "Should have resulted in a map."
                                    UpdateUnchanged -> pure () -- Fine
                                    r ->
                                        expectationFailure $
                                        "Should have been UpdateSuccess, was: " ++
                                        show r
