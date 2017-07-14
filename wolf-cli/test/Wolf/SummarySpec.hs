module Wolf.SummarySpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Command.Summary
import Wolf.OptParse.Types

import Wolf.Types.Gen ()

spec :: Spec
spec =
    describe "summary" $
    withSandbox $
    it "fails if no wolf repo has been initialised" $ \sb ->
        forAll genValid $ \person ->
            runReaderT (summary person) Settings {setWolfDir = sb} `shouldThrow`
            (\e -> e == ExitFailure 1)
