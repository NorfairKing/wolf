module Wolf.EntrySpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Cli.Command.Entry
import Wolf.Cli.OptParse.Types
import Wolf.Data.Entry
import Wolf.Data.Types

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
                             (personEntryProperties pe)) `shouldBe`
                    pe
        it "keeps all the new keys in the right order" $
            forAll genValid $ \now ->
                forAll genValid $ \pe ->
                    forAll genValid $ \strs ->
                        let pe' = reconstructPersonEntry now pe strs
                        in map fst (personEntryProperties pe') `shouldBe`
                           nub (map fst strs)
        it "produces valid PersonEntry's" $
            producesValidsOnValids3 reconstructPersonEntry
