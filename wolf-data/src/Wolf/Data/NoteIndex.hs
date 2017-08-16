{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.NoteIndex where

import Import

import Wolf.Data.JSONUtils
import Wolf.Data.Note.Types
import Wolf.Data.Path
import Wolf.Data.Types

-- | Retrieve a person's note index
getNoteIndex ::
       (MonadIO m, MonadReader DataSettings m) => PersonUuid -> m NoteIndex
getNoteIndex personUuid =
    noteIndexFile personUuid >>= readJSONWithDefault newNoteIndex

-- | Save a person's note index
putNoteIndex ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> NoteIndex
    -> m ()
putNoteIndex personUuid noteIndex = do
    i <- noteIndexFile personUuid
    writeJSON i noteIndex

-- | Look up a note in a note index
lookupInNoteIndex :: PersonNoteUuid -> NoteIndex -> Maybe PersonNoteUuid
lookupInNoteIndex noteUuid noteIndex =
    find (== noteUuid) $ noteIndexList noteIndex

-- | Create a new note in a note index.
-- The result is the new uuid and the new index
createNewNote ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> NoteIndex
    -> m (PersonNoteUuid, NoteIndex)
createNewNote person noteIndex = do
    noteUuid <- nextRandomPersonNoteUuid
    case lookupInNoteIndex noteUuid noteIndex of
        Nothing ->
            pure
                ( noteUuid
                , noteIndex
                  {noteIndexList = sort $ noteUuid : noteIndexList noteIndex})
        Just _ -> createNewNote person noteIndex -- Just try again

-- | Retrieve a given note, if it exists
readPersonNote ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> PersonNoteUuid
    -> m (Maybe PersonNote)
readPersonNote personUuid personNoteUuid = do
    pnf <- personNoteFile personUuid personNoteUuid
    readJSONWithMaybe pnf

-- | Get all notes' uuid's for a given person
getPersonNoteUuids ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> m [PersonNoteUuid]
getPersonNoteUuids personUuid = noteIndexList <$> getNoteIndex personUuid

-- | Retrieve all notes for a given person
getPersonNotes ::
       (MonadIO m, MonadReader DataSettings m) => PersonUuid -> m [PersonNote]
getPersonNotes personUuid = do
    nuuids <- getPersonNoteUuids personUuid
    catMaybes <$> mapM (readPersonNote personUuid) nuuids
