{-# LANGUAGE OverloadedStrings #-}

module Wolf.EntrySpec
    ( spec
    ) where

import TestImport
import TestUtils

import qualified Data.ByteString.Char8 as SB8

import Wolf.Data

import Wolf.Cli.Command.Entry
import Wolf.Cli.Command.Entry.Internal
import Wolf.Cli.OptParse.Types

import Wolf.Command.Entry.Gen ()
import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.Index.Types.Gen ()
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
    describe "parseEntryFileContents" $ do
        let p ls c =
                case parseEntryFileContents (SB8.unlines ls) of
                    Left err ->
                        expectationFailure $ "Parse failed: " <> show err
                    Right (ForEditor r) -> r `shouldBe` c
        it "parses this value correctly" $ p ["hi"] $ RVal "hi"
        it "parses this list correctly" $
            p ["- one", "- two", "- three"] $
            RList [RVal "one", RVal "two", RVal "three"]
        it "parses this map correctly" $
            p ["hello: world"] $ RMap [("hello", RVal "world")]
        it "parses this combination correctly" $
            p
                [ "first name: John"
                , "last name: Random"
                , "email:"
                , " - john@random.com"
                , " - contact@random.com"
                , "address:"
                , " street: First"
                , " town: RandomVille"
                ] $
            RMap
                [ ("first name", RVal "John")
                , ("last name", RVal "Random")
                , ( "email"
                  , RList [RVal "john@random.com", RVal "contact@random.com"])
                , ( "address"
                  , RMap
                        [("street", RVal "First"), ("town", RVal "RandomVille")])
                ]
