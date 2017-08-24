{-# LANGUAGE FlexibleContexts #-}

module Wolf.Server.Accounts
    ( getAccounts
    , lookupAccountUUID
    , tryToAddNewAccount
    , getAccount
    , storeAccount
    ) where

import Import

import qualified Data.Map as M
import Data.Map (Map)

import Wolf.API
import Wolf.Data.JSONUtils

import Wolf.Server.Path
import Wolf.Server.Types

getAccounts ::
       (MonadIO m, MonadReader WolfServerEnv m) => m (Map Text AccountUUID)
getAccounts = do
    af <- accountsFile
    readJSONWithDefault M.empty af

lookupAccountUUID ::
       (MonadIO m, MonadReader WolfServerEnv m) => Text -> m (Maybe AccountUUID)
lookupAccountUUID username = do
    as <- getAccounts
    pure $ M.lookup username as

tryToAddNewAccount ::
       (MonadIO m, MonadReader WolfServerEnv m) => Text -> m (Maybe AccountUUID)
tryToAddNewAccount username = do
    af <- accountsFile
    as <- getAccounts
    case M.lookup username as of
        Just _ -> pure Nothing
        Nothing -> do
            uuid <- liftIO newAccountUUID
            writeJSON af $ M.insert username uuid as
            pure $ Just uuid

getAccount ::
       (MonadIO m, MonadReader WolfServerEnv m)
    => AccountUUID
    -> m (Maybe Account)
getAccount uuid = do
    adf <- accountDataFile uuid
    readJSONWithMaybe adf

storeAccount :: (MonadIO m, MonadReader WolfServerEnv m) => Account -> m ()
storeAccount acc = do
    adf <- accountDataFile $ accountUUID acc
    writeJSON adf acc
