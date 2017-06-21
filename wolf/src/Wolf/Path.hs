{-# LANGUAGE FlexibleContexts #-}

module Wolf.Path where

import Import

import Wolf.OptParse.Types
import Wolf.Types

wolfDir :: MonadReader Settings m => m (Path Abs Dir)
wolfDir = asks setWolfDir

initFile :: (MonadReader Settings m, MonadIO m) => m (Path Abs File)
initFile = do
    wd <- wolfDir
    liftIO $ resolveFile wd "init.json"

indexFile :: (MonadReader Settings m, MonadIO m) => m (Path Abs File)
indexFile = do
    wd <- wolfDir
    liftIO $ resolveFile wd "index.json"

peopleDir :: (MonadReader Settings m, MonadIO m) => m (Path Abs Dir)
peopleDir = do
    wd <- wolfDir
    liftIO $ resolveDir wd "people"

personDir ::
       (MonadReader Settings m, MonadIO m) => PersonUuid -> m (Path Abs Dir)
personDir personUuid = do
    pd <- peopleDir
    liftIO $ resolveDir pd $ personUuidString personUuid

personEntryFile ::
       (MonadReader Settings m, MonadIO m) => PersonUuid -> m (Path Abs File)
personEntryFile personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveFile pd "entry.json"

tmpPersonEntryFile ::
       (MonadReader Settings m, MonadIO m) => PersonUuid -> m (Path Abs File)
tmpPersonEntryFile personUuid = do
    td <- liftIO getTempDir
    liftIO $ resolveFile td $ personUuidString personUuid ++ "-entry.wolf"

noteIndexFile ::
       (MonadReader Settings m, MonadIO m) => PersonUuid -> m (Path Abs File)
noteIndexFile personUuid = do
    wd <- personDir personUuid
    liftIO $ resolveFile wd "notes-index.json"

personNotesDir ::
       (MonadReader Settings m, MonadIO m) => PersonUuid -> m (Path Abs Dir)
personNotesDir personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveDir pd "notes"

personNoteFile ::
       (MonadReader Settings m, MonadIO m)
    => PersonUuid
    -> PersonNoteUuid
    -> m (Path Abs File)
personNoteFile personUuid personNoteUuid = do
    pnd <- personNotesDir personUuid
    liftIO $ resolveFile pnd $ personNoteUuidString personNoteUuid

tmpPersonNoteFile ::
       (MonadReader Settings m, MonadIO m)
    => PersonUuid
    -> PersonNoteUuid
    -> m (Path Abs File)
tmpPersonNoteFile personUuid personNoteUuid = do
    tmpDir <- liftIO getTempDir
    liftIO $
        resolveFile tmpDir $
        intercalate
            "-"
            [personUuidString personUuid, personNoteUuidString personNoteUuid]
