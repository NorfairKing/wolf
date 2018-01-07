{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Wolf.Web.Server.Handler.Suggestions where

import Import

import Yesod
import Yesod.Auth

import Wolf.Data

import Wolf.Web.Server.Foundation

getSuggestionsR :: Handler Html
getSuggestionsR = do
    ix <- runData getIndexWithDefault
    aSugs <- runData $ readUnusedSuggestions aliasSuggestionType
    withNavBar $ do
        setTitle "Wolf Suggestions"
        $(widgetFile "suggestions")
