module Wolf.NoteSpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Note
import Wolf.OptParse.Types

import Wolf.Types.Gen ()

spec :: Spec
spec =
    describe "note" $
    withSandbox $
    it "fails if no wolf repo has been initialised" $ \sb ->
        forAll genValid $ \person ->
            runReaderT (note person) Settings {setWolfDir = sb} `shouldThrow`
            (\e -> e == ExitFailure 1)
