{-# LANGUAGE OverloadedStrings #-}

module Wolf.EntrySpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Data

import Wolf.Cli.Command.Entry
import Wolf.Cli.OptParse.Types

import Wolf.Command.Entry.Gen ()
import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.Index.Types.Gen ()
import Wolf.Data.Types.Gen ()

spec :: Spec
spec =
    describe "entry" $
    withSandbox $
    it "fails if no wolf repo has been initialised" $ \sb ->
        forAll genValid $ \person ->
            runReaderT
                (entry person)
                Settings {setDataSets = DataSettings {dataSetWolfDir = sb}} `shouldThrow`
            (\e -> e == ExitFailure 1)
