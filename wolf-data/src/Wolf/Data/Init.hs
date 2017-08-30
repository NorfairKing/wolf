{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Init where

import Import

import Data.Time

import Wolf.Data.JSONUtils
import Wolf.Data.Path
import Wolf.Data.Types

-- | Initialise, or re-initialise a wolf repository.
--
-- If the wolf repository had already been initialised, this overwrites the
-- 'InitData' file.
initWolf :: (MonadIO m, MonadReader DataSettings m) => m ()
initWolf = do
    dir <- wolfDir
    ensureDir dir
    d <- genInitData
    writeInitData d

-- | Retrieve the 'InitData'
getInitData :: (MonadIO m, MonadReader DataSettings m) => m (Maybe InitData)
getInitData = initFile >>= readJSONWithMaybe

writeInitData :: (MonadIO m, MonadReader DataSettings m) => InitData -> m ()
writeInitData d = do
    iFile <- initFile
    writeJSON iFile d

-- | Generate a new 'InitData'
genInitData :: (MonadIO m, MonadReader DataSettings m) => m InitData
genInitData = do
    dir <- wolfDir
    now <- liftIO getCurrentTime
    pure InitData {initDataDir = dir, initTimestamp = now}

-- | Perform an action in a wolf repository,
-- but only if it has been initialised, otherwise this will 'die'.
withInitCheck :: (MonadIO m, MonadReader DataSettings m) => m () -> m ()
withInitCheck func = do
    iFile <- initFile
    mid <- readJSONWithDefault Nothing iFile
    wd <- wolfDir
    case mid of
        Just InitData {..} -> func
        _ ->
            liftIO $
            die $
            unwords
                [ "No wolf repository has been initialised in"
                , toFilePath wd
                , "yet."
                ]
