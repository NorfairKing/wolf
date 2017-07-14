module Wolf.GitSpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Command.Git
import Wolf.OptParse.Types
import Wolf.Types

import Wolf.Types.Gen ()

spec :: Spec
spec =
    describe "git" $
    withSandbox $
    it "fails if no wolf repo has been initialised" $ \sb ->
        forAll genValid $ \args ->
            runReaderT
                (git args)
                Settings {setDataSets = DataSettings {dataSetWolfDir = sb}} `shouldThrow`
            (\e -> e == ExitFailure 1)
