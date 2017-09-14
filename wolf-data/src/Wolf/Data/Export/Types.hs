{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types
    ( Export(..)
    ) where

import Import

import Data.Aeson
import qualified Data.Set as S

import Wolf.Data.Entry.Types
import Wolf.Data.Index.Types
import Wolf.Data.Init.Types
import Wolf.Data.Note.Types
import Wolf.Data.NoteIndex.Types
import Wolf.Data.People.Types
import Wolf.Data.Suggestion.Types

data Export = Export
    { exportInitData :: InitData
    , exportPersonIndex :: Index
    , exportPeople :: [PersonUuid]
    , exportPersonEntries :: [(PersonUuid, PersonEntry)]
    , exportNoteIndex :: NoteIndex
    , exportNoteIndices :: [(PersonUuid, NoteIndex)]
    , exportNotes :: [(NoteUuid, Note)]
    , exportEntrySuggestions :: [Suggestion EntrySuggestion]
    , exportUsedEntrySuggestions :: [Suggestion EntrySuggestion]
    } deriving (Show, Eq, Generic)

instance Validity Export where
    isValid Export {..} =
        and
            [ isValid exportInitData
            , isValid exportPersonIndex
            , isValid exportPeople
            , isValid exportPersonEntries
            , isValid exportNoteIndex
            , isValid exportNoteIndices
            , isValid exportNotes
            , isValid exportEntrySuggestions
            , isValid exportUsedEntrySuggestions
            , all (`elem` exportPeople) $ indexMap exportPersonIndex
            , all (`elem` exportPeople) $ map fst exportPersonEntries
            , all (`elem` exportPeople) $ map fst exportNoteIndices
            , S.fromList (map fst exportNotes) == noteIndexSet exportNoteIndex
            , all (`isSubNoteIndexOf` exportNoteIndex) $
              map snd exportNoteIndices
            , null $
              exportEntrySuggestions `intersect` exportUsedEntrySuggestions
            ]

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
