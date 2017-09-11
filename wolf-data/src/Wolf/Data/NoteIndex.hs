{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.NoteIndex
    ( NoteIndex(..)
    , newNoteIndex
    , NoteUuid
    , nextRandomNoteUuid
    , noteUuidText
    , noteUuidString
    -- * Manipulating indices purely
    , addToNoteIndex
    , containsNoteUuid
    -- * Manipulate the global note index
    , getNoteIndex
    , putNoteIndex
    -- ** Convenience functions for all notes
    , getNoteUuids
    , getNotes
    -- * Manipulate a person's note index
    , getPersonNoteIndex
    , putPersonNoteIndex
    -- ** Convenience functions for a person's notes
    , getPersonNoteUuids
    , getPersonNotes
    -- * Creating new notes, end-to-end
    , createNewNote
    , createNewNoteUuid
    ) where

import Import

import Wolf.Data.JSONUtils
import Wolf.Data.Note
import Wolf.Data.NoteIndex.Types
import Wolf.Data.Path
import Wolf.Data.People.Types
import Wolf.Data.Types

-- | Add a note uuid to a note index
--
-- Nothing if the note uuid already existed in the index
addToNoteIndex :: NoteIndex -> NoteUuid -> Maybe NoteIndex
addToNoteIndex ni@(NoteIndex uuids) nuuid =
    if ni `containsNoteUuid` nuuid
        then Nothing
        else Just $ NoteIndex $ nuuid : uuids

-- | Check if a given note index contains a given note uuid
containsNoteUuid :: NoteIndex -> NoteUuid -> Bool
containsNoteUuid noteIndex noteUuid = noteUuid `elem` noteIndexList noteIndex

-- | Retrieve the global note index
getNoteIndex :: (MonadIO m, MonadReader DataSettings m) => m NoteIndex
getNoteIndex = noteIndexFile >>= readJSONWithDefault newNoteIndex

-- | Save the global note index
putNoteIndex :: (MonadIO m, MonadReader DataSettings m) => NoteIndex -> m ()
putNoteIndex noteIndex = do
    i <- noteIndexFile
    writeJSON i noteIndex

-- | Retrieve all note uuids
getNoteUuids :: (MonadIO m, MonadReader DataSettings m) => m [NoteUuid]
getNoteUuids = noteIndexList <$> getNoteIndex

-- | Retrieve all notes
getNotes :: (MonadIO m, MonadReader DataSettings m) => m [Note]
getNotes = do
    nuuids <- getNoteUuids
    catMaybes <$> mapM readNote nuuids

-- | Retrieve a person's note index
getPersonNoteIndex ::
       (MonadIO m, MonadReader DataSettings m) => PersonUuid -> m NoteIndex
getPersonNoteIndex personUuid =
    personNoteIndexFile personUuid >>= readJSONWithDefault newNoteIndex

-- | Save a person's note index
putPersonNoteIndex ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> NoteIndex
    -> m ()
putPersonNoteIndex personUuid noteIndex = do
    i <- personNoteIndexFile personUuid
    writeJSON i noteIndex

-- | Get all notes' uuid's for a given person
getPersonNoteUuids ::
       (MonadIO m, MonadReader DataSettings m) => PersonUuid -> m [NoteUuid]
getPersonNoteUuids personUuid = noteIndexList <$> getPersonNoteIndex personUuid

-- | Retrieve all notes for a given person
getPersonNotes ::
       (MonadIO m, MonadReader DataSettings m) => PersonUuid -> m [Note]
getPersonNotes personUuid = do
    nuuids <- getPersonNoteUuids personUuid
    catMaybes <$> mapM readNote nuuids

-- | Create a new note, end to end.
--
-- This does all the relevant work:
-- - Generating a new UUID
-- - Adding the note uuid to the global note index
-- - Adding the note uuid to the right people's note indices
createNewNote :: (MonadIO m, MonadReader DataSettings m) => Note -> m NoteUuid
createNewNote n = do
    gni <- getNoteIndex
    pniTups <-
        forM (noteRelevantPeople n) $ \personUuid ->
            (,) personUuid <$> getPersonNoteIndex personUuid
    go gni pniTups
  where
    go gni pniTups = do
        (newUuid, newGlobalIndex) <- createNewNoteUuid gni
        let mnis =
                forM pniTups $ \(personUuid, personNoteIndex) ->
                    (,) personUuid <$> addToNoteIndex personNoteIndex newUuid
        case mnis of
            Nothing -> go gni pniTups -- Try generating another uuid
            Just newPnis -> do
                putNoteIndex newGlobalIndex
                mapM_ (uncurry putPersonNoteIndex) newPnis
                writeNote newUuid n
                pure newUuid

-- | Create a new note in a note index.
-- The result is the new uuid and the new index
createNewNoteUuid ::
       MonadIO m
    => NoteIndex -- ^ The global note index
    -> m (NoteUuid, NoteIndex)
createNewNoteUuid noteIndex = do
    noteUuid <- nextRandomNoteUuid
    if noteIndex `containsNoteUuid` noteUuid
        then createNewNoteUuid noteIndex -- Just try again
        else pure
                 ( noteUuid
                 , noteIndex
                   {noteIndexList = sort $ noteUuid : noteIndexList noteIndex})
