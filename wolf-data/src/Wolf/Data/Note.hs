{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Note
    ( readPersonNote
    , writePersonNote
    , PersonNote(..)
    ) where

import Import

import Wolf.Data.JSONUtils
import Wolf.Data.Note.Types
import Wolf.Data.Path
import Wolf.Data.Types

-- | Retrieve a given note, if it exists
readPersonNote ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> PersonNoteUuid
    -> m (Maybe PersonNote)
readPersonNote personUuid personNoteUuid = do
    pnf <- personNoteFile personUuid personNoteUuid
    readJSONWithMaybe pnf

-- | Write a given note
writePersonNote ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> PersonNoteUuid
    -> PersonNote
    -> m ()
writePersonNote personUuid personNoteUuid personNote = do
    nf <- personNoteFile personUuid personNoteUuid
    writeJSON nf personNote
