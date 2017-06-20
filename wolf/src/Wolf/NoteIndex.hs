{-# LANGUAGE FlexibleContexts #-}

module Wolf.NoteIndex where

import Import

import Wolf.JSONUtils
import Wolf.OptParse.Types
import Wolf.Path
import Wolf.Types

getNoteIndex :: (MonadIO m, MonadReader Settings m) => PersonUuid -> m NoteIndex
getNoteIndex personUuid =
    noteIndexFile personUuid >>= readJSONWithDefault newNoteIndex

putNoteIndex ::
       (MonadIO m, MonadReader Settings m) => PersonUuid -> NoteIndex -> m ()
putNoteIndex personUuid noteIndex = do
    i <- noteIndexFile personUuid
    writeJSON i noteIndex

lookupInNoteIndex :: PersonNoteUuid -> NoteIndex -> Maybe PersonNoteUuid
lookupInNoteIndex noteUuid noteIndex =
    find (== noteUuid) $ noteIndexList noteIndex

createNewNote ::
       (MonadIO m, MonadReader Settings m)
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

readPersonNote ::
       (MonadIO m, MonadReader Settings m)
    => PersonUuid
    -> PersonNoteUuid
    -> m (Maybe PersonNote)
readPersonNote personUuid personNoteUuid = do
    pnf <- personNoteFile personUuid personNoteUuid
    readJSONWithDefault Nothing pnf

getPersonNoteUuids ::
       (MonadIO m, MonadReader Settings m) => PersonUuid -> m [PersonNoteUuid]
getPersonNoteUuids personUuid = noteIndexList <$> getNoteIndex personUuid

getPersonNotes ::
       (MonadIO m, MonadReader Settings m) => PersonUuid -> m [PersonNote]
getPersonNotes personUuid = do
    nuuids <- getPersonNoteUuids personUuid
    catMaybes <$> mapM (readPersonNote personUuid) nuuids
