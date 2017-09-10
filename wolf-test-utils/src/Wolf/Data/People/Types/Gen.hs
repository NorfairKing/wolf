{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.People.Types.Gen where

import Import

import Data.UUID.Types

import Wolf.Data.People.Types

instance Validity UUID where
    isValid = const True

instance GenUnchecked UUID where
    genUnchecked =
        let genWord = fromIntegral <$> (genUnchecked :: Gen Int)
        in fromWords <$> genWord <*> genWord <*> genWord <*> genWord

instance GenValid UUID

instance GenUnchecked PersonUuid

instance GenValid PersonUuid where
    genValid = PersonUuid <$> genValid
