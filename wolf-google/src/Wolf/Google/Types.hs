{-# LANGUAGE DeriveGeneric #-}

module Wolf.Google.Types where

import Import

import Data.Aeson as JSON

data PersonSuggestion = PersonSuggestion
    { personSuggestionAlias :: Text
    , personSuggestionEntry :: JSON.Value
    } deriving (Show, Eq, Generic)

instance FromJSON PersonSuggestion

instance ToJSON PersonSuggestion
