{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Suggestion.Types.Gen where

import Import

import Data.Hashable

import Wolf.Data.Suggestion.Types

import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.Index.Types.Gen ()
import Wolf.Data.People.Types.Gen ()

instance GenUnchecked (SuggestionIndex a)

instance GenValid (SuggestionIndex a) where
  genValid = genValidStructurallyWithoutExtraChecking

instance GenUnchecked SuggestionType

instance GenValid SuggestionType where
  genValid = genValidStructurallyWithoutExtraChecking

instance GenUnchecked (SuggestionHash a)

instance GenValid (SuggestionHash a) where
  genValid = genValidStructurallyWithoutExtraChecking

instance GenUnchecked a => GenUnchecked (Suggestion a)

instance GenValid a => GenValid (Suggestion a) where
  genValid = genValidStructurallyWithoutExtraChecking

instance GenUnchecked AliasSuggestion

instance GenValid AliasSuggestion where
  genValid = genValidStructurallyWithoutExtraChecking

instance GenUnchecked EntrySuggestion

instance GenValid EntrySuggestion where
  genValid = genValidStructurallyWithoutExtraChecking

instance GenUnchecked SuggestionRepo

instance GenValid SuggestionRepo where
  genValid = genValidStructurallyWithoutExtraChecking

instance GenUnchecked a => GenUnchecked (SuggestionTypeRepo a)

instance (GenValid a, Hashable a) => GenValid (SuggestionTypeRepo a) where
  genValid = genValidStructurally
