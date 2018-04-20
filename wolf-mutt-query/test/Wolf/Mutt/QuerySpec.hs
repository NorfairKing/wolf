{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Mutt.QuerySpec
    ( spec
    ) where

import TestImport

import Wolf.Mutt.Query

import Wolf.Mutt.Query.Gen ()

spec :: Spec
spec =
    describe "formatSearchResult" $
    it "produces valid texts" $ producesValidsOnValids formatSearchResult
