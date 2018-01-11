{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Wolf.Web.Server.Handler.Suggestions where

import Import

import qualified Data.Set as S

import Yesod
import Yesod.Auth

import Wolf.Data

import Wolf.Web.Server.Foundation

import Wolf.Web.Server.Handler.Suggestions.Alias
import Wolf.Web.Server.Handler.Suggestions.Class

getSuggestionsR :: Handler Html
getSuggestionsR = do
    ix <- runData getIndexWithDefault
    aSugs <- runData $ readUnusedSuggestions aliasSuggestionType
    let sws = map suggestionWidget $ S.toList aSugs
    withNavBar $ do
        setTitle "Wolf Suggestions"
        $(widgetFile "suggestions")
