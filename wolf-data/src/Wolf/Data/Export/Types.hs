{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types
    ( Export(..)
    ) where

import Import

import Data.Aeson

import Wolf.Data.Entry.Types
import Wolf.Data.Index.Types
import Wolf.Data.Init.Types
import Wolf.Data.Note.Types
import Wolf.Data.NoteIndex.Types
import Wolf.Data.People.Types
import Wolf.Data.Suggestion.Types

data Export = Export
    { exportInitData :: InitData
    , exportPersonIndex :: Maybe Index
    , exportPeople :: [PersonUuid]
    , exportPersonEntries :: [(PersonUuid, PersonEntry)]
    , exportNoteIndex :: NoteIndex
    , exportNoteIndices :: [(PersonUuid, NoteIndex)]
    , exportNotes :: [Note]
    , exportEntrySuggestions :: [Suggestion EntrySuggestion]
    , exportUsedEntrySuggestions :: [Suggestion EntrySuggestion]
    } deriving (Show, Eq, Generic)

instance Validity Export

instance NFData Export

instance FromJSON Export where
    parseJSON =
        withObject "Export" $ \o ->
            Export <$> o .: "init-data" <*> o .: "person-index" <*>
            o .: "people" <*>
            o .: "person-entries" <*>
            o .: "note-index" <*>
            o .: "note-indices" <*>
            o .: "notes" <*>
            o .: "entry-suggestions" <*>
            o .: "used-entry-suggestions"

instance ToJSON Export where
    toJSON Export {..} =
        object
            [ "init-data" .= exportInitData
            , "person-index" .= exportPersonIndex
            , "people" .= exportPeople
            , "person-entries" .= exportPersonEntries
            , "note-index" .= exportNoteIndex
            , "note-indices" .= exportNoteIndices
            , "notes" .= exportNotes
            , "entry-suggestions" .= exportEntrySuggestions
            , "used-entry-suggestions" .= exportUsedEntrySuggestions
            ]
