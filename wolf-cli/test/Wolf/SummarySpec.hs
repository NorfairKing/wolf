module Wolf.SummarySpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Data

import Wolf.Cli.Command.Summary
import Wolf.Cli.OptParse.Types

import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.Note.Types.Gen ()
import Wolf.Data.Types.Gen ()

spec :: Spec
spec = do
    describe "summary" $
        withSandbox $
        it "fails if no wolf repo has been initialised" $ \sb ->
            forAll genValid $ \person ->
                runReaderT
                    (summary person)
                    Settings {setDataSets = DataSettings {dataSetWolfDir = sb}} `shouldThrow`
                (\e -> e == ExitFailure 1)
    describe "summaryReport" $
        it "produces valid summary reports" $
        producesValidsOnValids3 summaryReport
