{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Note.Types where

import Import

import Data.Aeson as JSON
import Data.Time

data PersonNote = PersonNote
    { personNoteContents :: Text
    , personNoteTimestamp :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity PersonNote

instance FromJSON PersonNote where
    parseJSON ob =
        (withObject "PersonNote" $ \o ->
             PersonNote <$> o .: "personNoteContents" <*>
             o .: "personNoteTimestamp")
            ob <|>
        (withObject "PersonNote" $ \o ->
             PersonNote <$> o .: "contents" <*> o .: "timestamp")
            ob

instance ToJSON PersonNote where
    toJSON PersonNote {..} =
        object
            [ "contents" .= personNoteContents
            , "timestamp" .= personNoteTimestamp
            ]
