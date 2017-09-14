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
            mi <- getIndex
            people <- getPersonUuids
            noteIndex <- getNoteIndex
            noteIxs <- mapM (\p -> (,) p <$> getPersonNoteIndex p) people
            notes <- getNotes
            entrySuggestions <- readPersonEntrySuggestions
            usedEntrySuggestions <- readUsedPersonEntrySuggestions
            pure $
                Just
                    Export
                    { exportInitData = initData
                    , exportPersonIndex = mi
                    , exportPeople = people
                    , exportNoteIndex = noteIndex
                    , exportNoteIndices = noteIxs
                    , exportNotes = notes
                    , exportEntrySuggestions = entrySuggestions
                    , exportUsedEntrySuggestions = usedEntrySuggestions
                    }
