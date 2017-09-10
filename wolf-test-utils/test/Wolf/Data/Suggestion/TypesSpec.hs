{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Suggestion.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Suggestion.Types

import Wolf.Data.Suggestion.Types.Gen ()

spec :: Spec
spec = do
    eqSpec @(Suggestion Double)
    genValidSpec @(Suggestion Double)
    jsonSpecOnValid @(Suggestion Double)
    eqSpec @EntrySuggestion
    genValidSpec @EntrySuggestion
    jsonSpecOnValid @EntrySuggestion
