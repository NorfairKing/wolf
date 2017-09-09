{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Suggestion where

import Import

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import System.Console.ANSI as ANSI

import Wolf.Data

import Wolf.Cli.Command.Entry.Internal (tmpEntryFileContents) -- TODO move this somewhere else
import Wolf.Cli.OptParse.Types
import Wolf.Cli.Report
import Wolf.Cli.Utils

suggestion :: (MonadIO m, MonadReader Settings m) => SuggestionSettings -> m ()
suggestion DispatchListSuggestions = do
    sugs <- runData readPersonEntrySuggestions
    liftIO $
        mapM_
            (putStr . renderReport . renderSuggestion renderEntrySuggestion)
            sugs

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
    case TE.decodeUtf8' $ tmpEntryFileContents entrySuggestionEntry of
        Left _ ->
            colored [SetColor Foreground Dull Red] "Failed to decode UTF8 Text"
        Right t -> stringReport $ T.unpack t
