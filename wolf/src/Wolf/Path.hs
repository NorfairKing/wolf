{-# LANGUAGE TemplateHaskell #-}

module Wolf.Path where

import Import

import Wolf.Types

wolfDir
    :: MonadIO m
    => m (Path Abs Dir)
wolfDir = (</> $(mkRelDir ".wolf")) <$> liftIO getHomeDir

indexFile
    :: MonadIO m
    => m (Path Abs File)
indexFile = do
    wd <- wolfDir
    liftIO $ resolveFile wd "index.json"

peopleDir
    :: MonadIO m
    => m (Path Abs Dir)
peopleDir = do
    wd <- wolfDir
    liftIO $ resolveDir wd "people"

personDir
    :: MonadIO m
    => PersonUuid -> m (Path Abs Dir)
personDir personUuid = do
    pd <- peopleDir
    liftIO $ resolveDir pd $ personUuidString personUuid

personEntryFile
    :: MonadIO m
    => PersonUuid -> m (Path Abs File)
personEntryFile personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveFile pd "entry.json"

noteIndexFile
    :: MonadIO m
    => PersonUuid -> m (Path Abs File)
noteIndexFile personUuid = do
    wd <- personDir personUuid
    liftIO $ resolveFile wd "notes-index.json"

personNotesDir
    :: MonadIO m
    => PersonUuid -> m (Path Abs Dir)
personNotesDir personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveDir pd "notes"

personNoteFile
    :: MonadIO m
    => PersonUuid -> PersonNoteUuid -> m (Path Abs File)
personNoteFile personUuid personNoteUuid = do
    pnd <- personNotesDir personUuid
    liftIO $ resolveFile pnd $ personNoteUuidString personNoteUuid

tmpPersonNoteFile
    :: MonadIO m
    => PersonUuid -> PersonNoteUuid -> m (Path Abs File)
tmpPersonNoteFile personUuid personNoteUuid = do
    tmpDir <- liftIO getTempDir
    liftIO $
        resolveFile tmpDir $
        intercalate
            "-"
            [personUuidString personUuid, personNoteUuidString personNoteUuid]
