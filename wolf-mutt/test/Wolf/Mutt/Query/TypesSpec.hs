{-# LANGUAGE TypeApplications #-}

module Wolf.Mutt.Query.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Mutt.Query.Types

import Wolf.Mutt.Query.Gen ()

spec :: Spec
spec = genValidSpec @SearchResult
