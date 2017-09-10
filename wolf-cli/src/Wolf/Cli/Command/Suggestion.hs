{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Suggestion where

import Import

import Wolf.Cli.Command.Suggestion.List
import Wolf.Cli.Command.Suggestion.Review
import Wolf.Cli.OptParse.Types

suggestion :: (MonadIO m, MonadReader Settings m) => SuggestionSettings -> m ()
suggestion DispatchListSuggestions = listSuggestions
suggestion DispatchReviewSuggestion = reviewSuggestion
