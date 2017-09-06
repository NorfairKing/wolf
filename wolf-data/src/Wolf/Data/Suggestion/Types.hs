{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Suggestion.Types where

import Import

import Data.Aeson

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
