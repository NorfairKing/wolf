{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Suggestion.TypesSpec
  ( spec
  ) where

import TestImport

import Wolf.Data.Suggestion.Types

import Wolf.Data.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @AliasSuggestion
  genValidSpec @AliasSuggestion
  jsonSpecOnValid @AliasSuggestion
  eqSpecOnValid @EntrySuggestion
  genValidSpec @EntrySuggestion
  jsonSpecOnValid @EntrySuggestion
  eqSpecOnValid @(SuggestionIndex Int)
  genValidSpec @(SuggestionIndex Int)
  jsonSpecOnValid @(SuggestionIndex Int)
  eqSpecOnValid @(SuggestionHash Int)
  genValidSpec @(SuggestionHash Int)
  jsonSpecOnValid @(SuggestionHash Int)
  eqSpecOnValid @(Suggestion Int)
  genValidSpec @(Suggestion Int)
  jsonSpecOnValid @(Suggestion Int)
  hashableSpecOnValid @(Suggestion Int)
  functorSpecOnValid @Suggestion
  eqSpecOnValid @(SuggestionTypeRepo Int)
  genValidSpec @(SuggestionTypeRepo Int)
  jsonSpecOnValid @(SuggestionTypeRepo Int)
  eqSpecOnValid @SuggestionRepo
  genValidSpec @SuggestionRepo
  jsonSpecOnValid @SuggestionRepo
