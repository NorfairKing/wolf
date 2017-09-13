{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Init.Types.Gen where

import Import

import Wolf.Data.Init.Types

instance GenUnchecked InitData

instance GenValid InitData where
    genValid = InitData <$> genValid <*> genValid
