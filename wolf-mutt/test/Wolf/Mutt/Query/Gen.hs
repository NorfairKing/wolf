{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Mutt.Query.Gen where

import TestImport

import Wolf.Mutt.Query.Types

instance GenValid SearchResult where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
