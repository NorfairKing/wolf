module Wolf.EntrySpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Data

import Wolf.Cli.Command.Entry
import Wolf.Cli.OptParse.Types

import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.Types.Gen ()

spec :: Spec
spec = do
    describe "entry" $
        withSandbox $
        it "fails if no wolf repo has been initialised" $ \sb ->
            forAll genValid $ \person ->
                runReaderT
                    (entry person)
                    Settings {setDataSets = DataSettings {dataSetWolfDir = sb}} `shouldThrow`
                (\e -> e == ExitFailure 1)
    describe "reconstructPersonEntry" $ do
        it "doesn't change anything if nothing changed" $
            forAll genValid $ \now ->
                forAll genValid $ \pe ->
                    reconstructPersonEntry
                        now
                        pe
                        (toRawYaml $ personEntryProperties pe) `shouldBe`
                    Just pe
        it "keeps all the new keys in the right order" $
            forAll genValid $ \now ->
                forAll genValid $ \pe ->
                    forAll genValid $ \ry ->
                        case reconstructPersonEntry now pe ry of
                            Nothing -> pure () -- That's fine.
                            Just pe' ->
                                case (personEntryProperties pe', ry) of
                                    (PMap t1s, RMap t2s) ->
                                        map fst t1s `shouldBe` map fst t2s
                                    _ -> pure () -- Fine, for now
        it "produces valid PersonEntry's" $
            producesValidsOnValids3 reconstructPersonEntry
    describe "parseEntryFileContents" $ do
        it
            "successfully parses unchanged file contents from 'tmpEntryFileContents'" $
            forAll genValid $ \pe -> do
                let contents = tmpEntryFileContents pe
                case parseEntryFileContents contents of
                    Left err ->
                        expectationFailure $
                        "Failed to parse contents: " <> show err
                    Right _ -> pure () -- Fine
        it
            "successfully parses unchanged file contents from 'tmpEntryFileContents' with the right properties" $
            forAll genValid $ \pe -> do
                let contents = tmpEntryFileContents pe
                case parseEntryFileContents contents of
                    Left err ->
                        expectationFailure $
                        "Failed to parse contents: " <> show err
                    Right (ForEditor ry) ->
                        ry `shouldBe` toRawYaml (personEntryProperties pe)

instance GenUnchecked RawYaml

instance GenValid RawYaml
