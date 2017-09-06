{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Suggestion.Types.Gen where

import Import

import Wolf.Data.Suggestion.Types

instance GenUnchecked a => GenUnchecked (Suggestion a)

instance GenValid a => GenValid (Suggestion a) where
    genValid = Suggestion <$> genValid <*> genValid <*> genValid
