module Wolf.ReviewSpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Data

import Wolf.Cli.Command.Review
import Wolf.Cli.OptParse.Types

import Wolf.Data.Types.Gen ()

spec :: Spec
spec =
    describe "review" $
    withSandbox $
    it "fails if no wolf repo has been initialised" $ \sb ->
        runReaderT
            review
            Settings {setDataSets = DataSettings {dataSetWolfDir = sb}} `shouldThrow`
        (\e -> e == ExitFailure 1)
