{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types
    ( Repo(..)
    ) where

import Import

import Data.Aeson
import qualified Data.Map as M

import Wolf.Data.Entry.Types
import Wolf.Data.Index.Types
import Wolf.Data.Init.Types
import Wolf.Data.Note.Types
import Wolf.Data.NoteIndex.Types
import Wolf.Data.People.Types
import Wolf.Data.Suggestion.Types

data Repo = Repo
    { repoInitData :: InitData
    , repoPersonIndex :: Index
    , repoPersonEntries :: Map PersonUuid PersonEntry
    , repoNoteIndex :: NoteIndex
    , repoNoteIndices :: Map PersonUuid NoteIndex
    , repoNotes :: Map NoteUuid Note
    , repoEntrySuggestions :: [Suggestion EntrySuggestion]
    , repoUsedEntrySuggestions :: [Suggestion EntrySuggestion]
    } deriving (Show, Eq, Generic)

instance Validity Repo where
    isValid Repo {..} =
        and
            [ isValid repoInitData
            , isValid repoPersonIndex
            , isValid repoPersonEntries
            , isValid repoNoteIndex
            , isValid repoNoteIndices
            , isValid repoNotes
            , isValid repoEntrySuggestions
            , isValid repoUsedEntrySuggestions
            , M.keysSet repoNotes == noteIndexSet repoNoteIndex
            , all (`isSubNoteIndexOf` repoNoteIndex) $ M.elems repoNoteIndices
            , null $ repoEntrySuggestions `intersect` repoUsedEntrySuggestions
            ]
    validate Repo {..} =
        mconcat
            [ repoInitData <?!> "repoInitData"
            , repoPersonIndex <?!> "repoPersonIndex"
            , repoPersonEntries <?!> "repoPersonEntries"
            , repoNoteIndex <?!> "repoNoteIndex"
            , repoNoteIndices <?!> "repoNoteIndices"
            , repoNotes <?!> "repoNotes"
            , repoEntrySuggestions <?!> "repoEntrySuggestions"
            , repoUsedEntrySuggestions <?!> "repoUsedEntrySuggestions"
            , M.keysSet repoNotes ==
              noteIndexSet repoNoteIndex <?@>
              "The key set of repoNotes equals the note UUID's in the global note index."
            , mconcat $
              flip map (M.toList repoNoteIndices) $ \(personUuid, noteIndex) ->
                  noteIndex `isSubNoteIndexOf` repoNoteIndex <?@>
                  unlines
                      [ "The person note index for person"
                      , personUuidString personUuid
                      , "is a sub-noteindex of the global note index."
                      , "Person note index: " ++ show noteIndex
                      , "Global note index: " ++ show repoNoteIndex
                      ]
            , null (repoEntrySuggestions `intersect` repoUsedEntrySuggestions) <?@>
              "The entry suggestions and the used entry suggestions should not share suggestions."
            ]

instance NFData Repo

instance FromJSON Repo where
    parseJSON =
        withObject "Repo" $ \o ->
            Repo <$> o .: "init-data" <*> o .: "person-index" <*>
            o .: "person-entries" <*>
            o .: "note-index" <*>
            o .: "note-indices" <*>
            o .: "notes" <*>
            o .: "entry-suggestions" <*>
            o .: "used-entry-suggestions"

instance ToJSON Repo where
    toJSON Repo {..} =
        object
            [ "init-data" .= repoInitData
            , "person-index" .= repoPersonIndex
            , "person-entries" .= repoPersonEntries
            , "note-index" .= repoNoteIndex
            , "note-indices" .= repoNoteIndices
            , "notes" .= repoNotes
            , "entry-suggestions" .= repoEntrySuggestions
            , "used-entry-suggestions" .= repoUsedEntrySuggestions
            ]
