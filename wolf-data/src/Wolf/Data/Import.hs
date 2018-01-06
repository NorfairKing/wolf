{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Import
    ( importRepo
    ) where

import Import

import qualified Data.Map as M

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
    void $ M.traverseWithKey putPersonEntry repoPersonEntries
    putNoteIndex repoNoteIndex
    void $ M.traverseWithKey putPersonNoteIndex repoNoteIndices
    void $ M.traverseWithKey writeNote repoNotes
    writeSuggestions entrySuggestionType repoEntrySuggestions
    writeUsedSuggestions entrySuggestionType repoUsedEntrySuggestions
