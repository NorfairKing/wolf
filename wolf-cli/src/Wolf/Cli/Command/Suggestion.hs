{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Suggestion where

import Import

import Wolf.Cli.OptParse.Types

suggestion :: (MonadIO m, MonadReader Settings m) => SuggestionSettings -> m ()
suggestion DispatchListSuggestions = pure ()
