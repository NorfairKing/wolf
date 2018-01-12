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
    suggestionPreviewWidget = aliasSuggestionPreviewWidget
    suggestionWidget = aliasSuggestionWidget

aliasSuggestionPreviewWidget ::
       SuggestionUuid -> Suggestion AliasSuggestion -> Widget
aliasSuggestionPreviewWidget uuid sug =
    let d = suggestionData sug
    in $(widgetFile "suggestions/alias/preview")

aliasSuggestionWidget :: SuggestionUuid -> AliasSuggestion -> Widget
aliasSuggestionWidget uuid asug = $(widgetFile "suggestions/alias/full")
