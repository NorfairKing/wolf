module Wolf.Web.Server.Handler.Suggestions.Class where

import Import

import Yesod

import Wolf.Data

import Wolf.Web.Server.Foundation

class DisplaySuggestion a where
    suggestionWidget :: Suggestion a -> WolfWidget
