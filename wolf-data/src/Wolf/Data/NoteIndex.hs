{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.NoteIndex
  ( NoteIndex
  , noteIndexSet
  , newNoteIndex
  , NoteUuid
    -- * Manipulating indices purely
  , addToNoteIndex
  , containsNoteUuid
  , isSubNoteIndexOf
    -- * Manipulate the global note index
  , getNoteIndex
  , putNoteIndex
    -- ** Convenience functions for all notes
  , getNoteUuids
  , getNotes
    -- * Manipulate a person's note index
  , getPersonNoteIndex
  , getPersonNoteIndexWithDefault
  , putPersonNoteIndex
    -- ** Convenience functions for a person's notes
  , getPersonNoteUuids
  , getPersonNotes
    -- * Creating new notes, end-to-end
  , createNewNote
  , createNewNoteUuid
  ) where

import Import

import qualified Data.Set as S

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
addToNoteIndex ni nuuid =
  if ni `containsNoteUuid` nuuid
    then Nothing
    else Just $ NoteIndex $ S.insert nuuid $ noteIndexSet ni

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
getNoteUuids = (toList . noteIndexSet) <$> getNoteIndex

-- | Retrieve all notes
getNotes :: (MonadIO m, MonadReader DataSettings m) => m [Note]
getNotes = do
  nuuids <- getNoteUuids
  catMaybes <$> mapM readNote nuuids

-- | Retrieve a person's note index
getPersonNoteIndexWithDefault ::
     (MonadIO m, MonadReader DataSettings m) => PersonUuid -> m NoteIndex
getPersonNoteIndexWithDefault personUuid =
  personNoteIndexFile personUuid >>= readJSONWithDefault newNoteIndex

getPersonNoteIndex ::
     (MonadIO m, MonadReader DataSettings m)
  => PersonUuid
  -> m (Maybe NoteIndex)
getPersonNoteIndex personUuid =
  personNoteIndexFile personUuid >>= readJSONWithMaybe

-- | Save a person's note index
putPersonNoteIndex ::
     (MonadIO m, MonadReader DataSettings m) => PersonUuid -> NoteIndex -> m ()
putPersonNoteIndex personUuid noteIndex = do
  i <- personNoteIndexFile personUuid
  writeJSON i noteIndex

-- | Get all notes' uuid's for a given person
getPersonNoteUuids ::
     (MonadIO m, MonadReader DataSettings m) => PersonUuid -> m [NoteUuid]
getPersonNoteUuids personUuid =
  (toList . noteIndexSet) <$> getPersonNoteIndexWithDefault personUuid

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
-- - Writing the note to a file
createNewNote :: (MonadIO m, MonadReader DataSettings m) => Note -> m NoteUuid
createNewNote n = do
  gni <- getNoteIndex
  pniTups <-
    forM (toList $ noteRelevantPeople n) $ \personUuid ->
      (,) personUuid <$> getPersonNoteIndexWithDefault personUuid
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
  noteUuid <- nextRandomUUID
  case addToNoteIndex noteIndex noteUuid of
    Nothing -> createNewNoteUuid noteIndex
    Just newIndex -> pure (noteUuid, newIndex)
