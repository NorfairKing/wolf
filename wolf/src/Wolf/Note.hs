module Wolf.Note where

import Import

import Wolf.Editor
import Wolf.Index
import Wolf.JSONUtils
import Wolf.Path
import Wolf.Types

getPersonEntry
    :: MonadIO m
    => PersonUuid -> m PersonEntry
getPersonEntry personUuid =
    personEntryFile personUuid >>= readJSONWithDefault newPersonEntry

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

note
    :: MonadIO m
    => String -> m ()
note person = do
    origIndex <- getIndex
    liftIO $ print origIndex
    (personUuid, index) <- lookupOrCreateNewPerson person origIndex
    liftIO $ print personUuid
    liftIO $ print index
    personEntry <- getPersonEntry personUuid
    liftIO $ print personEntry
    origNoteIndex <- getNoteIndex personUuid
    (noteUuid, noteIndex) <- createNewNote personUuid origNoteIndex
    liftIO $ print noteUuid
    liftIO $ print noteIndex
    editingResult <- startNoteEditor personUuid noteUuid
    case editingResult of
        EditingFailure reason ->
            liftIO $
            putStrLn $
            unwords
                ["ERROR: failed to edit the note file:", reason, ",not saving."]
        EditingSuccess -> do
            putIndex index
            putNoteIndex personUuid noteIndex

startNoteEditor
    :: MonadIO m
    => PersonUuid -> PersonNoteUuid -> m EditingResult
startNoteEditor personUuid noteUuid = do
    liftIO $ print ("Starting note editor for:", personUuid, noteUuid)
    nf <- personNoteFile personUuid noteUuid
    startEditorOn nf
