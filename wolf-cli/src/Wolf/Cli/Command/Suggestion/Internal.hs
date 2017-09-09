{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Suggestion.Internal
    ( renderSuggestion
    , renderEntrySuggestion
    ) where

import Import

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import System.Console.ANSI as ANSI

import Wolf.Data

import Wolf.Cli.Command.Entry.Internal (tmpEntryFileContents) -- TODO move this somewhere else
import Wolf.Cli.Report

renderSuggestion :: (a -> Report) -> Suggestion a -> Report
renderSuggestion func Suggestion {..} =
    unlinesReport
        [ colored [SetColor Foreground Dull Blue] $
          "Suggestor: " ++ T.unpack suggestionSuggestor
        , colored [SetColor Foreground Dull Yellow] $
          "Reason: " ++ T.unpack suggestionReason
        , func suggestionData
        ]

renderEntrySuggestion :: EntrySuggestion -> Report
renderEntrySuggestion EntrySuggestion {..} =
    unlinesReport $
    [ colored [SetColor Foreground Dull Green] $ "Suggested alias: " ++ (T.unpack alias)
    | alias <- entrySuggestionNewAliases
    ] ++
    [ case TE.decodeUtf8' $ tmpEntryFileContents entrySuggestionEntry of
          Left _ ->
              colored
                  [SetColor Foreground Dull Red]
                  "Failed to decode UTF8 Text for entry suggestion YAML"
          Right t -> stringReport $ T.unpack t
    ]
