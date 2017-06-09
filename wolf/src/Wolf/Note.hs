module Wolf.Note where

import Import

import Wolf.Editor
import Wolf.Index
import Wolf.NoteIndex
import Wolf.Path
import Wolf.Types

note
    :: MonadIO m
    => String -> m ()
note person = do
    origIndex <- getIndex
    (personUuid, index) <- lookupOrCreateNewPerson person origIndex
    origNoteIndex <- getNoteIndex personUuid
    (noteUuid, noteIndex) <- createNewNote personUuid origNoteIndex
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
    nf <- personNoteFile personUuid noteUuid
    startEditorOn nf
