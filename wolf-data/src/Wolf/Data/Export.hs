{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Export
    ( Repo
    , repoInitData
    , repoPersonIndex
    , exportRepo
    ) where

import Import

import Wolf.Data.Export.Types
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.People
import Wolf.Data.Suggestion
import Wolf.Data.Types

exportRepo :: (MonadIO m, MonadReader DataSettings m) => m (Maybe Repo)
exportRepo = do
    mid <- readInitData
    case mid of
        Nothing -> pure Nothing
        Just initData -> do
            mi <- getIndexWithDefault
            people <- getPersonUuids
            entries <-
                mapMaybe (\(p, e) -> (,) p <$> e) <$>
                mapM (\p -> (,) p <$> getPersonEntry p) people
            noteIndex <- getNoteIndex
            noteIxs <- mapM (\p -> (,) p <$> getPersonNoteIndex p) people
            noteUuids <- getNoteUuids
            notes <-
                mapMaybe (\(p, e) -> (,) p <$> e) <$>
                mapM (\uuid -> (,) uuid <$> readNote uuid) noteUuids
            entrySuggestions <- readPersonEntrySuggestions
            usedEntrySuggestions <- readUsedPersonEntrySuggestions
            pure $
                Just
                    Repo
                    { repoInitData = initData
                    , repoPersonIndex = mi
                    , repoPersonEntries = entries
                    , repoNoteIndex = noteIndex
                    , repoNoteIndices = noteIxs
                    , repoNotes = notes
                    , repoEntrySuggestions = entrySuggestions
                    , repoUsedEntrySuggestions = usedEntrySuggestions
                    }
