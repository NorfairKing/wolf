{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types
    ( Repo(..)
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

data Repo = Repo
    { repoInitData :: InitData
    , repoPersonIndex :: Index
    , repoPeople :: [PersonUuid]
    , repoPersonEntries :: [(PersonUuid, PersonEntry)]
    , repoNoteIndex :: NoteIndex
    , repoNoteIndices :: [(PersonUuid, NoteIndex)]
    , repoNotes :: [(NoteUuid, Note)]
    , repoEntrySuggestions :: [Suggestion EntrySuggestion]
    , repoUsedEntrySuggestions :: [Suggestion EntrySuggestion]
    } deriving (Show, Eq, Generic)

instance Validity Repo where
    isValid Repo {..} =
        and
            [ isValid repoInitData
            , isValid repoPersonIndex
            , isValid repoPeople
            , isValid repoPersonEntries
            , isValid repoNoteIndex
            , isValid repoNoteIndices
            , isValid repoNotes
            , isValid repoEntrySuggestions
            , isValid repoUsedEntrySuggestions
            , all (`elem` repoPeople) $ indexMap repoPersonIndex
            , all (`elem` repoPeople) $ map fst repoPersonEntries
            , all (`elem` repoPeople) $ map fst repoNoteIndices
            , S.fromList (map fst repoNotes) == noteIndexSet repoNoteIndex
            , all (`isSubNoteIndexOf` repoNoteIndex) $ map snd repoNoteIndices
            , null $ repoEntrySuggestions `intersect` repoUsedEntrySuggestions
            ]

instance NFData Repo

instance FromJSON Repo where
    parseJSON =
        withObject "Repo" $ \o ->
            Repo <$> o .: "init-data" <*> o .: "person-index" <*> o .: "people" <*>
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
            , "people" .= repoPeople
            , "person-entries" .= repoPersonEntries
            , "note-index" .= repoNoteIndex
            , "note-indices" .= repoNoteIndices
            , "notes" .= repoNotes
            , "entry-suggestions" .= repoEntrySuggestions
            , "used-entry-suggestions" .= repoUsedEntrySuggestions
            ]
