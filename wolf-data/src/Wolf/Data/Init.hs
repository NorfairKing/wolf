{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Init
    ( InitData
    , initDataDir
    , initTimestamp
    , initWolf
    , readInitData
    , writeInitData
    , genInitData
    , withInitCheck
    , withInitCheck_
    ) where

import Import

import Data.Time

import Wolf.Data.Init.Types
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
readInitData :: (MonadIO m, MonadReader DataSettings m) => m (Maybe InitData)
readInitData = initFile >>= readJSONWithMaybe

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
withInitCheck ::
       (MonadIO m, MonadReader DataSettings m) => (InitData -> m a) -> m a
withInitCheck func = do
    iFile <- initFile
    mid <- readJSONWithDefault Nothing iFile
    wd <- wolfDir
    case mid of
        Just dat -> func dat
        _ ->
            liftIO $
            die $
            unwords
                [ "No wolf repository has been initialised in"
                , toFilePath wd
                , "yet."
                ]

withInitCheck_ :: (MonadIO m, MonadReader DataSettings m) => m () -> m ()
withInitCheck_ = withInitCheck . const
