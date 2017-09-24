{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.People.Types.Gen where

import Import

import Wolf.Data.People.Types

instance GenUnchecked PersonUuid

instance GenValid PersonUuid where
    genValid = PersonUuid <$> genValid
