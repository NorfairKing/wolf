module Wolf.EntrySpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Entry
import Wolf.OptParse.Types
import Wolf.Types

import Wolf.Types.Gen ()

spec :: Spec
spec = do
    describe "entry" $
        withSandbox $
        it "fails if no wolf repo has been initialised" $ \sb ->
            forAll genValid $ \person ->
                runReaderT (entry person) Settings {setWolfDir = sb} `shouldThrow`
                (\e -> e == ExitFailure 1)
    describe "reconstructPersonEntry" $ do
        it "doesn't change anything if nothing changed" $
            forAll genValid $ \now ->
                forAll genValid $ \pe ->
                    let pe' =
                            reconstructPersonEntry
                                now
                                pe
                                (map (second personPropertyValueContents)
                                     (personEntryProperties pe))
                    in personEntryLastUpdatedTimestamp pe' `shouldBe`
                       personEntryLastUpdatedTimestamp pe
        it "keeps all the new keys in the right order" $
            forAll genValid $ \now ->
                forAll genValid $ \pe ->
                    forAll genValid $ \strs ->
                        let pe' = reconstructPersonEntry now pe strs
                        in map fst (personEntryProperties pe') `shouldBe`
                           nub (map fst strs)
        it "produces valid PersonEntry's" $
            producesValidsOnValids3 reconstructPersonEntry
