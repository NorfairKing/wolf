{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Wolf.Server.Path
  ( dataDir
  , accountsFile
  , accountsDir
  , accountDir
  , accountDataDir
  , accountDataFile
  ) where

import Import

import Wolf.API
import Wolf.Server.Types

dataDir :: MonadReader WolfServerEnv m => m (Path Abs Dir)
dataDir = asks wseDataDir

accountsFile :: MonadReader WolfServerEnv m => m (Path Abs File)
accountsFile = (</> $(mkRelFile "accounts.json")) <$> dataDir

accountsDir :: MonadReader WolfServerEnv m => m (Path Abs Dir)
accountsDir = (</> $(mkRelDir "accounts")) <$> dataDir

accountDir ::
     (MonadIO m, MonadReader WolfServerEnv m) => AccountUUID -> m (Path Abs Dir)
accountDir uuid = do
  ad <- accountsDir
  liftIO $ resolveDir ad $ uuidString uuid

accountDataDir ::
     (MonadIO m, MonadReader WolfServerEnv m) => AccountUUID -> m (Path Abs Dir)
accountDataDir uuid = (</> $(mkRelDir "data")) <$> accountDir uuid

accountDataFile ::
     (MonadIO m, MonadReader WolfServerEnv m)
  => AccountUUID
  -> m (Path Abs File)
accountDataFile uuid = (</> $(mkRelFile "account.json")) <$> accountDir uuid
