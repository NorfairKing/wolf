{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Import
    ( importRepo
    ) where

import Import

import Wolf.Data.Entry
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
    forM_ repoPersonEntries $ \(puuid, pindex) -> putPersonEntry puuid pindex
    putNoteIndex repoNoteIndex
    forM_ repoNoteIndices $ \(puuid, nindex) -> putPersonNoteIndex puuid nindex
    forM_ repoNotes $ \(nuuid, note) -> writeNote nuuid note
    writePersonEntrySuggestions repoEntrySuggestions
    writeUsedPersonEntrySuggestions repoUsedEntrySuggestions
