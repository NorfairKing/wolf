{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Index.Types.Gen where

import Import

import Wolf.Data.Index.Types

import Wolf.Data.People.Types.Gen ()

instance GenValid Alias where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Index where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
