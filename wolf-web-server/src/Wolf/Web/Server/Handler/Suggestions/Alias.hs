{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Web.Server.Handler.Suggestions.Alias where

import Import

import Yesod

import Wolf.Data
import Wolf.Data.Baked

import Wolf.Web.Server.Foundation

import Wolf.Web.Server.Handler.Suggestions.Class

postSuggestionsAliasRunR :: Handler Html
postSuggestionsAliasRunR = do
  runData makeAliasSuggestions
  redirect SuggestionsR

instance DisplaySuggestion AliasSuggestion where
  suggestionPreviewWidget = aliasSuggestionPreviewWidget
  suggestionWidget = aliasSuggestionWidget

aliasSuggestionPreviewWidget ::
     SuggestionUuid -> Suggestion AliasSuggestion -> Handler Widget
aliasSuggestionPreviewWidget uuid sug = do
  let d = suggestionData sug
  cf <- completionForm uuid $ suggestionData sug
  pure $(widgetFile "suggestions/alias/preview")

aliasSuggestionWidget :: SuggestionUuid -> AliasSuggestion -> Handler Widget
aliasSuggestionWidget uuid asug@AliasSuggestion {..} = do
  mpe <- runData $ getPersonEntry aliasSuggestionPerson
  as <-
    runData $ do
      ix <- getIndexWithDefault
      pure $ reverseIndexLookup aliasSuggestionPerson ix
  cf <- completionForm uuid asug
  pure $(widgetFile "suggestions/alias/full")

completionForm :: SuggestionUuid -> AliasSuggestion -> Handler Widget
completionForm uuid AliasSuggestion {..} = do
  token <- genToken
  pure $(widgetFile "suggestions/alias/completion-form")

data CompleteAliasSuggestion =
  CompleteAliasSuggestion
    { completeAliasSuggestionUuid :: SuggestionUuid
    , completeAliasSuggestionAgree :: Agreement
    }
  deriving (Show, Eq)

completeAliasSuggestionForm :: FormInput Handler CompleteAliasSuggestion
completeAliasSuggestionForm =
  CompleteAliasSuggestion <$> ireq hiddenField "suggestion" <*>
  ireq (checkMMap (pure . parseAgreement) renderAgreement hiddenField) "agree"

postSuggestionsAliasCompleteR :: Handler Html
postSuggestionsAliasCompleteR = do
  result <- runInputPostResult completeAliasSuggestionForm
  case result of
    FormSuccess CompleteAliasSuggestion {..} -> do
      runData $
        completeAliasSuggestion
          completeAliasSuggestionUuid
          completeAliasSuggestionAgree
      setMessage "Alias suggestion completed."
      redirect SuggestionsR
    r -> do
      liftIO $ print r
      setMessage "Something went wrong with the suggestion."
      redirect SuggestionsR
