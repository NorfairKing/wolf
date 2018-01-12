{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Wolf.Web.Server.Handler.Suggestions where

import Import

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time

import Yesod
import Yesod.Auth

import Wolf.Data
import Wolf.Data.Time

import Wolf.Web.Server.Foundation

import Wolf.Web.Server.Handler.Suggestions.Alias
import Wolf.Web.Server.Handler.Suggestions.Class

getSuggestionsR :: Handler Html
getSuggestionsR = do
    ix <- runData getIndexWithDefault
    aSugs <-
        runData $ readUnusedSuggestions aliasSuggestionType :: Handler (M.Map SuggestionUuid (Suggestion AliasSuggestion))
    let sws = map (uncurry suggestionPreviewWidget) $ M.toList aSugs
    withNavBar $ do
        setTitle "Wolf Suggestions"
        $(widgetFile "suggestions")

getSuggestionR :: FilePath -> SuggestionUuid -> Handler Html
getSuggestionR fp uuid =
    case parseSuggestionType fp of
        Nothing -> notFound -- TODO better error
        Just typ -> do
            mw <-
                if | typ == aliasSuggestionType ->
                       runData $ readSuggestionWidget @AliasSuggestion typ uuid
                   | otherwise -> pure Nothing
            case mw of
                Nothing -> notFound -- TODO better error
                Just (s, w) -> do
                    now <- liftIO getCurrentTime
                    withNavBar $ do
                        setTitle "Wolf Suggestion"
                        $(widgetFile "suggestion")

readSuggestionWidget ::
       forall a m.
       ( Hashable a
       , FromJSON a
       , DisplaySuggestion a
       , MonadIO m
       , MonadReader DataSettings m
       )
    => SuggestionType
    -> SuggestionUuid
    -> m (Maybe (Suggestion (), WolfWidget))
readSuggestionWidget typ uuid = do
    ms <- readSuggestion @a typ uuid
    case ms of
        Nothing -> pure Nothing
        Just s ->
            pure $ Just (() <$ s, suggestionWidget uuid $ suggestionData s)
