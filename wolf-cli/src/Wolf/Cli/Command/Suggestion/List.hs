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
listSuggestions = do
    sugs <- runData readPersonEntrySuggestions
    liftIO $
        mapM_
            (putStr . renderReport . renderSuggestion renderEntrySuggestion)
            sugs
