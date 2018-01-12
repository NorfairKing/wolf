{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Suggestion.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Suggestion.Types

import Wolf.Data.Gen ()

spec :: Spec
spec = do
    eqSpec @(Suggestion Double)
    genValidSpec @(Suggestion Double)
    jsonSpecOnValid @(Suggestion Double)
    functorSpecOnValid @Suggestion
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
