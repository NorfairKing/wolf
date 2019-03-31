{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Mutt.Query.Gen where

import TestImport

import Wolf.Mutt.Query.Types

instance GenUnchecked SearchResult

instance GenValid SearchResult where
  genValid = SearchResult <$> genValid <*> genValid <*> genValid
