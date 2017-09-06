{-# LANGUAGE DeriveGeneric #-}

module Wolf.Data.Suggestion.Types where

import Import

import Data.Aeson

data Suggestion a = Suggestion
    { suggestionSuggestor :: Text
    , suggestionReason :: Text
    , suggestionData :: a
    } deriving (Show, Eq, Generic)

instance Validity a => Validity (Suggestion a)

instance FromJSON a => FromJSON (Suggestion a)

instance ToJSON a => ToJSON (Suggestion a)
