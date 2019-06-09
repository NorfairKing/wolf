{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Entry.Types.Gen where

import Import

import Wolf.Data.Entry.Types

instance GenValid PersonEntry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PersonProperty where
  genValid = oneof [PVal <$> genValid, PList <$> genValid, (PMap <$> genValid) `suchThat` isValid]
  shrinkValid pp =
    case pp of
      PVal v -> PVal <$> shrinkValid v
      PList l -> PList <$> shrinkValid l
      PMap m -> filter isValid (PMap <$> shrinkValid m)

instance GenValid PersonPropertyValue where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
