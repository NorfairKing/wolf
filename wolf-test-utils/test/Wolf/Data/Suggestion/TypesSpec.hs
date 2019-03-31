{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Suggestion.TypesSpec
  ( spec
  ) where

import TestImport

import Wolf.Data.Suggestion.Types

import Wolf.Data.Gen ()

spec :: Spec
spec = do
  eqSpec @AliasSuggestion
  genValidSpec @AliasSuggestion
  jsonSpecOnValid @AliasSuggestion
  eqSpecOnValid @EntrySuggestion
  genValidSpec @EntrySuggestion
  jsonSpecOnValid @EntrySuggestion
  eqSpec @(SuggestionIndex Double)
  genValidSpec @(SuggestionIndex Double)
  jsonSpecOnValid @(SuggestionIndex Double)
  eqSpec @(SuggestionHash Double)
  genValidSpec @(SuggestionHash Double)
  jsonSpecOnValid @(SuggestionHash Double)
  eqSpecOnValid @(Suggestion Double)
  genValidSpec @(Suggestion Double)
  jsonSpecOnValid @(Suggestion Double)
  hashableSpecOnValid @(Suggestion Double)
  functorSpecOnValid @Suggestion
  eqSpecOnValid @(SuggestionTypeRepo Int)
  genValidSpec @(SuggestionTypeRepo Int)
  jsonSpecOnValid @(SuggestionTypeRepo Int)
  eqSpecOnValid @SuggestionRepo
  genValidSpec @SuggestionRepo
  jsonSpecOnValid @SuggestionRepo
