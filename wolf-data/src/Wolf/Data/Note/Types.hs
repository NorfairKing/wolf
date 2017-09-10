{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Note.Types where

import Import

import Data.Aeson as JSON
import Data.Time

import Wolf.Data.People.Types

data Note = Note
    { noteContents :: Text
    , noteTimestamp :: UTCTime
    , noteRelevantPeople :: [PersonUuid] -- TODO make this a set
    } deriving (Show, Eq, Ord, Generic)

instance Validity Note

instance FromJSON Note where
    parseJSON ob =
        (withObject "Note" $ \o ->
             Note <$> o .: "personNoteContents" <*> o .: "personNoteTimestamp" <*>
             pure [])
            ob <|>
        (withObject "Note" $ \o ->
             Note <$> o .: "contents" <*> o .: "timestamp" <*>
             o .:? "relevant-people" .!= [])
            ob

instance ToJSON Note where
    toJSON Note {..} =
        object
            [ "contents" .= noteContents
            , "timestamp" .= noteTimestamp
            , "relevant-people" .= noteRelevantPeople
            ]
