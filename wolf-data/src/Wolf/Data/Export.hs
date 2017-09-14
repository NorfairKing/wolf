{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Export
    ( Export
    , exportInitData
    , exportPersonIndex
    , export
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

export :: (MonadIO m, MonadReader DataSettings m) => m (Maybe Export)
export = do
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
                    Export
                    { exportInitData = initData
                    , exportPersonIndex = mi
                    , exportPersonEntries = entries
                    , exportPeople = people
                    , exportNoteIndex = noteIndex
                    , exportNoteIndices = noteIxs
                    , exportNotes = notes
                    , exportEntrySuggestions = entrySuggestions
                    , exportUsedEntrySuggestions = usedEntrySuggestions
                    }
