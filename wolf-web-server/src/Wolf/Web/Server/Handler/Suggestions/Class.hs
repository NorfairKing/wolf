module Wolf.Web.Server.Handler.Suggestions.Class where

import Import

import Yesod

import qualified System.FilePath as FP

import Wolf.Data

import Wolf.Web.Server.Foundation

suggestionR :: SuggestionType -> SuggestionUuid -> Route App
suggestionR (SuggestionType rf) =
    SuggestionR (FP.dropTrailingPathSeparator (fromRelDir rf))

class DisplaySuggestion a where
    suggestionPreviewWidget :: SuggestionUuid -> Suggestion a -> WolfWidget
    suggestionWidget :: SuggestionUuid -> a -> WolfWidget
