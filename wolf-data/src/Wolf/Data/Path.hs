{-# LANGUAGE FlexibleContexts #-}
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
    , tmpPersonEntryFile
    , personNoteIndexFile
    , noteFile
    , personNotesDir
    , personNotesFile
    , personNoteFile
    , tmpNoteFile
    ) where

import Import

import qualified Data.Text as T

import Wolf.Data.Types

wolfDir :: MonadReader DataSettings m => m (Path Abs Dir)
wolfDir = asks dataSetWolfDir

initFile :: (MonadReader DataSettings m, MonadIO m) => m (Path Abs File)
initFile = do
    wd <- wolfDir
    liftIO $ resolveFile wd "init.json"

indexFile :: (MonadReader DataSettings m, MonadIO m) => m (Path Abs File)
indexFile = do
    wd <- wolfDir
    liftIO $ resolveFile wd "index.json"

noteIndexFile :: (MonadReader DataSettings m, MonadIO m) => m (Path Abs File)
noteIndexFile = do
    wd <- wolfDir
    liftIO $ resolveFile wd "notes.json"

peopleDir :: (MonadReader DataSettings m, MonadIO m) => m (Path Abs Dir)
peopleDir = do
    wd <- wolfDir
    liftIO $ resolveDir wd "people"

notesDir :: (MonadReader DataSettings m, MonadIO m) => m (Path Abs Dir)
notesDir = do
    wd <- wolfDir
    liftIO $ resolveDir wd "notes"

personDir ::
       (MonadReader DataSettings m, MonadIO m) => PersonUuid -> m (Path Abs Dir)
personDir personUuid = do
    pd <- peopleDir
    liftIO $ resolveDir pd $ T.unpack $ personUuidText personUuid

personEntryFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> m (Path Abs File)
personEntryFile personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveFile pd "entry.json"

tmpPersonEntryFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> m (Path Abs File)
tmpPersonEntryFile personUuid = do
    td <- liftIO getTempDir
    liftIO $
        resolveFile td $ T.unpack (personUuidText personUuid) ++ "-entry.wolf"

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
    liftIO $ resolveFile nd $ T.unpack $ noteUuidText noteUuid

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
    liftIO $ resolveFile pnd $ T.unpack $ noteUuidText noteUuid

tmpNoteFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> NoteUuid
    -> m (Path Abs File)
tmpNoteFile personUuid noteUuid = do
    tmpDir <- liftIO getTempDir
    liftIO $
        resolveFile tmpDir $
        T.unpack $
        T.intercalate "-" [personUuidText personUuid, noteUuidText noteUuid]
