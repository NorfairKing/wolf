module Wolf.SummarySpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Data

import Wolf.Cli.Command.Summary
import Wolf.Cli.OptParse.Types

import Wolf.Data.Gen ()

spec :: Spec
spec = do
    describe "summary" $
        withSandbox $
        it "fails if no wolf repo has been initialised" $ \sb ->
            once $
            forAllValid $ \person ->
                runReaderT
                    (summary person)
                    Settings {setDataSets = DataSettings {dataSetWolfDir = sb}} `shouldThrow`
                (\e -> e == ExitFailure 1)
    describe "summaryReport" $
        it "produces valid summary reports" $
        forAllValid $ \a -> producesValidsOnValids3 $ summaryReport a
