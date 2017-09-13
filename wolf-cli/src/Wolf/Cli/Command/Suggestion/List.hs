{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Suggestion.List where

import Import

import Wolf.Data

import Wolf.Cli.Command.Suggestion.Internal
       (renderEntrySuggestion, renderSuggestion)
import Wolf.Cli.OptParse.Types
import Wolf.Cli.Report
import Wolf.Cli.Utils

listSuggestions :: (MonadIO m, MonadReader Settings m) => m ()
listSuggestions =
    runData $
    withInitCheck_ $ do
        sugs <- readPersonEntrySuggestions
        liftIO $ mapM_ (putReport . renderSuggestion renderEntrySuggestion) sugs
