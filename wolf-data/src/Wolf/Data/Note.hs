{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Note
    ( readNote
    , writeNote
    , Note(..)
    ) where

import Import

import Wolf.Data.JSONUtils
import Wolf.Data.Note.Types
import Wolf.Data.NoteIndex.Types
import Wolf.Data.Path
import Wolf.Data.Types

-- | Retrieve a given note, if it exists
readNote ::
       (MonadIO m, MonadReader DataSettings m) => NoteUuid -> m (Maybe Note)
readNote noteUuid = noteFile noteUuid >>= readJSONWithMaybe

-- | Write a given note.
--
-- WARNING: This only writes the note to its file, it does not ensure that all
-- the uuid references are correct in the appropriate indices.
writeNote :: (MonadIO m, MonadReader DataSettings m) => NoteUuid -> Note -> m ()
writeNote noteUuid note = do
    nf <- noteFile noteUuid
    writeJSON nf note
