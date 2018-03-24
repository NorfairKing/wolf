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
    eqSpec @EntrySuggestion
    genValidSpec @EntrySuggestion
    jsonSpecOnValid @EntrySuggestion
    eqSpec @(SuggestionIndex Double)
    genValidSpec @(SuggestionIndex Double)
    jsonSpecOnValid @(SuggestionIndex Double)
    eqSpec @(SuggestionHash Double)
    genValidSpec @(SuggestionHash Double)
    jsonSpecOnValid @(SuggestionHash Double)
    eqSpec @(Suggestion Double)
    genValidSpec @(Suggestion Double)
    jsonSpecOnValid @(Suggestion Double)
    hashableSpecOnValid @(Suggestion Double)
    functorSpecOnValid @Suggestion
    eqSpec @(SuggestionTypeRepo Int)
    genValidSpec @(SuggestionTypeRepo Int)
    jsonSpecOnValid @(SuggestionTypeRepo Int)
    eqSpec @SuggestionRepo
    genValidSpec @SuggestionRepo
    jsonSpecOnValid @SuggestionRepo
