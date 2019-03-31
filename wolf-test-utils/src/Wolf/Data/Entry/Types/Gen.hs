{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Entry.Types.Gen where

import Import

import Wolf.Data.Entry.Types

instance GenUnchecked PersonEntry

instance GenValid PersonEntry where
  genValid = PersonEntry <$> genValid

instance GenUnchecked PersonProperty

instance GenValid PersonProperty where
  genValid =
    oneof
      [ PVal <$> genValid
      , PList <$> genValid
      , (PMap <$> genValid) `suchThat` isValid
      ]

instance GenUnchecked PersonPropertyValue

instance GenValid PersonPropertyValue where
  genValid = PersonPropertyValue <$> genValid <*> genValid
