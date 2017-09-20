{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Import
    ( importRepo
    ) where

import Import

import Wolf.Data.Export.Types
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.Suggestion
import Wolf.Data.Types

importRepo :: (MonadIO m, MonadReader DataSettings m) => Repo -> m ()
importRepo Repo {..} = do
    writeInitData repoInitData
    putIndex repoPersonIndex
    mapM_ (uncurry putPersonEntry) repoPersonEntries
    putNoteIndex repoNoteIndex
    mapM_ (uncurry putPersonNoteIndex) repoNoteIndices
    mapM_ (uncurry writeNote) repoNotes
    writePersonEntrySuggestions repoEntrySuggestions
    writeUsedPersonEntrySuggestions repoUsedEntrySuggestions
