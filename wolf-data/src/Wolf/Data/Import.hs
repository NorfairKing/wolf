{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Import
    ( importRepo
    ) where

import Import

import Wolf.Data.Export.Types
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Entry
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.Suggestion
import Wolf.Data.Types

importRepo :: (MonadIO m, MonadReader DataSettings m) => Export -> m ()
importRepo Export {..} = do
    writeInitData exportInitData
    putIndex exportPersonIndex
    forM_ exportPersonEntries $ \(puuid, pindex) -> putPersonEntry puuid pindex
    putNoteIndex exportNoteIndex
    forM_ exportNoteIndices $ \(puuid, nindex) ->
        putPersonNoteIndex puuid nindex
    forM_ exportNotes $ \(nuuid, note) -> writeNote nuuid note
    writePersonEntrySuggestions exportEntrySuggestions
    writeUsedPersonEntrySuggestions exportUsedEntrySuggestions
