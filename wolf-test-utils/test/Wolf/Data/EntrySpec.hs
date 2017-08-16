module Wolf.Data.EntrySpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Entry

import Wolf.Data.Entry.Types.Gen ()

spec :: Spec
spec = do
    describe "personEntry" $
        it "only generates valid PersonEntry's" $ producesValid personEntry
    describe "personEntryTuples" $
        it "only generates valid lists" $
        producesValidsOnValids personEntryTuples
