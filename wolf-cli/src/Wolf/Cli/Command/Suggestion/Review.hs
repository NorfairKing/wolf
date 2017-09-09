{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Suggestion.Review
    ( reviewSuggestion
    ) where

import Import

import Wolf.Data

import Wolf.Cli.Command.Suggestion.Internal
       (renderEntrySuggestion, renderSuggestion)
import Wolf.Cli.OptParse.Types
import Wolf.Cli.Report
import Wolf.Cli.Utils

reviewSuggestion :: (MonadIO m, MonadReader Settings m) => m ()
reviewSuggestion = do
    sugs <- runData readPersonEntrySuggestions
    case sugs of
        [] -> liftIO $ putStrLn "No suggestions to review."
        (sug:_) -> reviewSingle sug

reviewSingle ::
       (MonadIO m, MonadReader Settings m) => Suggestion EntrySuggestion -> m ()
reviewSingle s@Suggestion {..} = do
    let EntrySuggestion {..} = suggestionData
    liftIO $ putStr $ renderReport $ renderSuggestion renderEntrySuggestion s
