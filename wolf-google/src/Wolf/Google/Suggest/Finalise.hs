{-# LANGUAGE OverloadedStrings #-}

module Wolf.Google.Suggest.Finalise
    ( finaliseSuggestion
    ) where

import Import

import qualified Data.Text as T

import Wolf.Data

finaliseSuggestion :: EntrySuggestion -> Suggestion EntrySuggestion
finaliseSuggestion es =
    Suggestion
    { suggestionSuggestor = "Google Contacts"
    , suggestionReason = reason
    , suggestionData = es
    }
  where
    reason =
        case entrySuggestionLikelyRelevantPerson es of
            Nothing -> "This data was exported"
            Just (uuid, score) ->
                T.concat
                    [ "This data was exported and suggested to be related to the person with UUID "
                    , uuidText uuid
                    , " and score "
                    , T.pack $ show score
                    , "."
                    ]
