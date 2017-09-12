{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Suggestion.Internal
    ( renderSuggestion
    , renderEntrySuggestion
    ) where

import Import

import System.Console.ANSI as ANSI

import Wolf.Data

import Wolf.Cli.Report

renderSuggestion :: (a -> Report) -> Suggestion a -> Report
renderSuggestion func Suggestion {..} =
    unlinesReport
        [ colored [SetColor Foreground Dull Blue] $
          "Suggestor: " <> suggestionSuggestor
        , colored [SetColor Foreground Dull Yellow] $
          "Reason: " <> suggestionReason
        , func suggestionData
        ]

renderEntrySuggestion :: EntrySuggestion -> Report
renderEntrySuggestion EntrySuggestion {..} =
    unlinesReport $
    (case entrySuggestionNewAliases of
         [] -> [green "No suggested aliases."]
         _ ->
             [ green $ "Suggested alias: " <> aliasText a
             | a <- entrySuggestionNewAliases
             ]) ++
    [textReport $ entryContents entrySuggestionEntry]
  where
    green = colored [SetColor Foreground Dull Green]
