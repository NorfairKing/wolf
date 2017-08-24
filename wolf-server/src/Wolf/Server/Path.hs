{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Wolf.Server.Path
    ( dataDir
    , accountsDir
    , accountDir
    , accountDataDir
    , accountDataFile
    ) where

import Import

import Control.Monad.Reader

import Wolf.Data.Types
import Wolf.Server.Types
import Wolf.API

dataDir :: MonadReader WolfServerEnv m => m (Path Abs Dir)
dataDir = asks wseDataDir

accountsDir :: MonadReader WolfServerEnv m => m (Path Abs Dir)
accountsDir = (</> $(mkRelDir "accounts")) <$> dataDir

accountDir ::
       (MonadIO m, MonadReader WolfServerEnv m)
    => AccountUUID
    -> m (Path Abs Dir)
accountDir uuid = do
    ad <- accountsDir
    liftIO $ resolveDir ad $ accountUUIDString uuid

accountDataDir ::
       (MonadIO m, MonadReader WolfServerEnv m)
    => AccountUUID
    -> m (Path Abs Dir)
accountDataDir uuid = (</> $(mkRelDir "data")) <$> accountDir uuid

accountDataFile ::
       (MonadIO m, MonadReader WolfServerEnv m)
    => AccountUUID
    -> m (Path Abs File)
accountDataFile uuid = (</> $(mkRelFile "account.json")) <$> accountDir uuid
