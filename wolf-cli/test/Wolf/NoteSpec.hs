module Wolf.NoteSpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Command.Note
import Wolf.OptParse.Types
import Wolf.Types

import Wolf.Types.Gen ()

spec :: Spec
spec =
    describe "note" $
    withSandbox $
    it "fails if no wolf repo has been initialised" $ \sb ->
        forAll genValid $ \person ->
            runReaderT
                (note person)
                Settings {setDataSets = DataSettings {dataSetWolfDir = sb}} `shouldThrow`
            (\e -> e == ExitFailure 1)
