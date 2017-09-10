{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Suggestion.Types where

import Import

import Data.Aeson

import Wolf.Data.Entry.Types
import Wolf.Data.Types

data Suggestion a = Suggestion
    { suggestionSuggestor :: Text
    , suggestionReason :: Text
    , suggestionData :: a
    } deriving (Show, Eq, Generic)

instance Validity a => Validity (Suggestion a)

instance FromJSON a => FromJSON (Suggestion a) where
    parseJSON =
        withObject "Suggestion" $ \o ->
            Suggestion <$> o .: "suggestor" <*> o .: "reason" <*> o .: "data"

instance ToJSON a => ToJSON (Suggestion a) where
    toJSON Suggestion {..} =
        object
            [ "suggestor" .= suggestionSuggestor
            , "reason" .= suggestionReason
            , "data" .= suggestionData
            ]

data EntrySuggestion = EntrySuggestion
    { entrySuggestionEntry :: PersonEntry
    , entrySuggestionNewAliases :: [Text]
    , entrySuggestionLikelyRelevantPerson :: Maybe (PersonUuid, Double)
    } deriving (Show, Eq, Generic)

instance Validity EntrySuggestion

instance FromJSON EntrySuggestion where
    parseJSON =
        withObject "EntrySuggestion" $ \o ->
            EntrySuggestion <$> o .: "entry" <*> o .: "new-aliases" <*>
            o .: "relevant-person"

instance ToJSON EntrySuggestion where
    toJSON EntrySuggestion {..} =
        object
            [ "entry" .= entrySuggestionEntry
            , "new-aliases" .= entrySuggestionNewAliases
            , "relevant-person" .= entrySuggestionLikelyRelevantPerson
            ]
