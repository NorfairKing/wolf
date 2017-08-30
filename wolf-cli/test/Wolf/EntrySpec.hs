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
                        (map (second personPropertyValueContents)
                             (personEntryTuples pe)) `shouldBe`
                    Just pe
        it "keeps all the new keys in the right order" $
            forAll genValid $ \now ->
                forAll genValid $ \pe ->
                    forAll genValid $ \strs ->
                        case reconstructPersonEntry now pe strs of
                            Nothing -> pure () -- That's fine.
                            Just pe' ->
                                map fst (personEntryTuples pe') `shouldBe`
                                nub (map fst strs)
        it "produces valid PersonEntry's" $
            producesValidsOnValids3 reconstructPersonEntry
