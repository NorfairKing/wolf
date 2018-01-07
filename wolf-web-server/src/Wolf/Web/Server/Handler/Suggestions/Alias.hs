module Wolf.Web.Server.Handler.Suggestions.Alias where

import Import

import Yesod
import Yesod.Auth

import Wolf.Data.Baked

import Wolf.Web.Server.Foundation

postSuggestionsRunAliasR :: Handler Html
postSuggestionsRunAliasR = do
    runData makeAliasSuggestions
    redirect SuggestionsR
