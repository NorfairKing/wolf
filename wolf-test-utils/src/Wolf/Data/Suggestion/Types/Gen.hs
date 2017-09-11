{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Suggestion.Types.Gen where

import Import

import Wolf.Data.Suggestion.Types

import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.Index.Types.Gen ()
import Wolf.Data.People.Types.Gen ()

instance GenUnchecked a => GenUnchecked (Suggestion a)

instance GenValid a => GenValid (Suggestion a) where
    genValid = Suggestion <$> genValid <*> genValid <*> genValid

instance GenUnchecked EntrySuggestion

instance GenValid EntrySuggestion where
    genValid = EntrySuggestion <$> genValid <*> genValid <*> genValid
