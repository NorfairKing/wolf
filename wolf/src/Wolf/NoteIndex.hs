module Wolf.NoteIndex where

import Import

import qualified Data.Text.IO as T

import Wolf.JSONUtils
import Wolf.Path
import Wolf.Types

getNoteIndex
    :: MonadIO m
    => PersonUuid -> m NoteIndex
getNoteIndex personUuid =
    noteIndexFile personUuid >>= readJSONWithDefault newNoteIndex

putNoteIndex
    :: MonadIO m
    => PersonUuid -> NoteIndex -> m ()
putNoteIndex personUuid noteIndex = do
    i <- noteIndexFile personUuid
    writeJSON i noteIndex

lookupInNoteIndex :: PersonNoteUuid -> NoteIndex -> Maybe PersonNoteUuid
lookupInNoteIndex noteUuid noteIndex =
    find (== noteUuid) $ noteIndexList noteIndex

createNewNote
    :: MonadIO m
    => PersonUuid -> NoteIndex -> m (PersonNoteUuid, NoteIndex)
createNewNote person noteIndex = do
    noteUuid <- nextRandomPersonNoteUuid
    case lookupInNoteIndex noteUuid noteIndex of
        Nothing ->
            pure
                ( noteUuid
                , noteIndex
                  {noteIndexList = sort $ noteUuid : noteIndexList noteIndex})
        Just _ -> createNewNote person noteIndex -- Just try again

readPersonNote
    :: MonadIO m
    => PersonUuid -> PersonNoteUuid -> m (Maybe PersonNote)
readPersonNote personUuid personNoteUuid = do
    pnf <- personNoteFile personUuid personNoteUuid
    contents <- liftIO (forgivingAbsence $ T.readFile $ toFilePath pnf)
    pure $ PersonNote <$> contents

getPersonNoteUuids
    :: MonadIO m
    => PersonUuid -> m [PersonNoteUuid]
getPersonNoteUuids personUuid = noteIndexList <$> getNoteIndex personUuid

getPersonNotes
    :: MonadIO m
    => PersonUuid -> m [PersonNote]
getPersonNotes personUuid = do
    nuuids <- getPersonNoteUuids personUuid
    catMaybes <$> mapM (readPersonNote personUuid) nuuids
