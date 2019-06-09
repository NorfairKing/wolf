{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Baked.Gen where

import TestImport

import Wolf.Data.Baked

instance GenValid Met where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Gender where
  genValid = frequency [(20, pure Male), (20, pure Female), (1, Other <$> genValid)]
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Name where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
