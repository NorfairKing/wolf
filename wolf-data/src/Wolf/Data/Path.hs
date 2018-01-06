{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Path
    ( wolfDir
    , initFile
    , indexFile
    , noteIndexFile
    , peopleDir
    , notesDir
    , personDir
    , personEntryFile
    , personNoteIndexFile
    , noteFile
    , personNotesDir
    , personNotesFile
    , personNoteFile
    ) where

import Import

import qualified Data.Text as T

import Wolf.Data.NoteIndex.Types
import Wolf.Data.People.Types
import Wolf.Data.Types

wolfDir :: MonadReader DataSettings m => m (Path Abs Dir)
wolfDir = asks dataSetWolfDir

initFile :: MonadReader DataSettings m => m (Path Abs File)
initFile = (</> $(mkRelFile "init.json")) <$> wolfDir

indexFile :: MonadReader DataSettings m => m (Path Abs File)
indexFile = (</> $(mkRelFile "index.json")) <$> wolfDir

noteIndexFile :: MonadReader DataSettings m => m (Path Abs File)
noteIndexFile = (</> $(mkRelFile "notes.json")) <$> wolfDir

peopleDir :: MonadReader DataSettings m => m (Path Abs Dir)
peopleDir = (</> $(mkRelDir "people")) <$> wolfDir

notesDir :: MonadReader DataSettings m => m (Path Abs Dir)
notesDir = (</> $(mkRelDir "notes")) <$> wolfDir

personDir ::
       (MonadReader DataSettings m, MonadIO m) => PersonUuid -> m (Path Abs Dir)
personDir personUuid = do
    pd <- peopleDir
    liftIO $ resolveDir pd $ T.unpack $ uuidText personUuid

personEntryFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> m (Path Abs File)
personEntryFile personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveFile pd "entry.json"

personNoteIndexFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> m (Path Abs File)
personNoteIndexFile personUuid = do
    wd <- personDir personUuid
    liftIO $ resolveFile wd "notes-index.json"

noteFile ::
       (MonadReader DataSettings m, MonadIO m) => NoteUuid -> m (Path Abs File)
noteFile noteUuid = do
    nd <- notesDir
    liftIO $ resolveFile nd $ T.unpack $ uuidText noteUuid

personNotesDir ::
       (MonadReader DataSettings m, MonadIO m) => PersonUuid -> m (Path Abs Dir)
personNotesDir personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveDir pd "notes"

personNotesFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> m (Path Abs File)
personNotesFile personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveFile pd "notes.json"

personNoteFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> NoteUuid
    -> m (Path Abs File)
personNoteFile personUuid noteUuid = do
    pnd <- personNotesDir personUuid
    liftIO $ resolveFile pnd $ T.unpack $ uuidText noteUuid
