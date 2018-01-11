{-# LANGUAGE TemplateHaskell #-}

module Wolf.Web.Server.Handler.Suggestions.Alias where

import Import

import Yesod
import Yesod.Auth

import Wolf.Data
import Wolf.Data.Baked

import Wolf.Web.Server.Foundation

import Wolf.Web.Server.Handler.Suggestions.Class

postSuggestionsRunAliasR :: Handler Html
postSuggestionsRunAliasR = do
    runData makeAliasSuggestions
    redirect SuggestionsR

instance DisplaySuggestion AliasSuggestion where
    suggestionWidget = aliasSuggestionWidget

aliasSuggestionWidget :: Suggestion AliasSuggestion -> Widget
aliasSuggestionWidget sug =
    let d = suggestionData sug
    in $(widgetFile "suggestions/alias")
