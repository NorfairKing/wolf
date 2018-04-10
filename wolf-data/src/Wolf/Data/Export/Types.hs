{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types
    ( Repo(..)
    , Result(..)
    , ResultS(..)
    , Exporter(..)
    ) where

import Import

import Data.Aeson hiding (Result)
import qualified Data.Map as M

import Wolf.Data.Entry.Types
import Wolf.Data.Export.Result
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
    , repoSuggestions :: SuggestionRepo
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
            , M.keysSet repoNotes == noteIndexSet repoNoteIndex
            , all (`isSubNoteIndexOf` repoNoteIndex) $ M.elems repoNoteIndices
            , isValid repoSuggestions
            ]
    validate Repo {..} =
        mconcat
            [ repoInitData <?!> "repoInitData"
            , repoPersonIndex <?!> "repoPersonIndex"
            , repoPersonEntries <?!> "repoPersonEntries"
            , repoNoteIndex <?!> "repoNoteIndex"
            , repoNoteIndices <?!> "repoNoteIndices"
            , repoNotes <?!> "repoNotes"
            , M.keysSet repoNotes ==
              noteIndexSet repoNoteIndex <?@>
              "The key set of repoNotes equals the note UUID's in the global note index."
            , mconcat $
              flip map (M.toList repoNoteIndices) $ \(personUuid, noteIndex) ->
                  noteIndex `isSubNoteIndexOf` repoNoteIndex <?@>
                  unlines
                      [ "The person note index for person"
                      , uuidString personUuid
                      , "is a sub-noteindex of the global note index."
                      , "Person note index: " ++ show noteIndex
                      , "Global note index: " ++ show repoNoteIndex
                      ]
            , repoSuggestions <?!> "repoSuggestions"
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
            o .: "suggestions"

instance ToJSON Repo where
    toJSON Repo {..} =
        object
            [ "init-data" .= repoInitData
            , "person-index" .= repoPersonIndex
            , "person-entries" .= repoPersonEntries
            , "note-index" .= repoNoteIndex
            , "note-indices" .= repoNoteIndices
            , "notes" .= repoNotes
            , "suggestions" .= repoSuggestions
            ]

type Warning = String

type Error = String

type ResultS = Result Warning Error

type Exporter = ResultS Repo
