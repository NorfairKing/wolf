module Wolf.SummarySpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Cli.Command.Summary
import Wolf.Cli.OptParse.Types
import Wolf.Data.Types

import Wolf.Data.Types.Gen ()

spec :: Spec
spec =
    describe "summary" $
    withSandbox $
    it "fails if no wolf repo has been initialised" $ \sb ->
        forAll genValid $ \person ->
            runReaderT
                (summary person)
                Settings {setDataSets = DataSettings {dataSetWolfDir = sb}} `shouldThrow`
            (\e -> e == ExitFailure 1)
