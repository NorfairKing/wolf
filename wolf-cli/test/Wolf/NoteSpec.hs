module Wolf.NoteSpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Data

import Wolf.Cli.Command.Note
import Wolf.Cli.OptParse.Types

import Wolf.Data.Gen ()

spec :: Spec
spec =
    describe "note" $
    withSandbox $
    it "fails if no wolf repo has been initialised" $ \sb ->
        once $
        forAllValid $ \person ->
            runReaderT
                (note person)
                Settings {setDataSets = DataSettings {dataSetWolfDir = sb}} `shouldThrow`
            (\e -> e == ExitFailure 1)
