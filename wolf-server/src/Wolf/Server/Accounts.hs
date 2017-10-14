{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Server.Accounts
    ( RegisterError(..)
    , registerAccount
    , getAccounts
    , storeAccounts
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

data RegisterError
    = InvalidPassword
    | UsernameExists
    deriving (Show, Eq)

registerAccount ::
       (MonadIO m, MonadReader WolfServerEnv m)
    => Register
    -> m (Either RegisterError AccountUUID)
registerAccount Register {..} = do
    mh <- liftIO $ hashPassword registerPassword
    case mh of
        Nothing -> pure $ Left InvalidPassword
        Just ph -> do
            muuid <- tryToAddNewAccount registerUsername
            case muuid of
                Nothing -> pure $ Left UsernameExists
                Just uuid -> do
                    let acc =
                            Account
                            { accountUUID = uuid
                            , accountUsername = registerUsername
                            , accountPasswordHash = ph
                            }
                    storeAccount acc
                    pure $ Right uuid

-- | Retrieve global accounts data
getAccounts ::
       (MonadIO m, MonadReader WolfServerEnv m) => m (Map Username AccountUUID)
getAccounts = do
    af <- accountsFile
    readJSONWithDefault M.empty af

-- | Store global accounts data
storeAccounts ::
       (MonadIO m, MonadReader WolfServerEnv m)
    => Map Username AccountUUID
    -> m ()
storeAccounts accs = do
    af <- accountsFile
    writeJSON af accs

lookupAccountUUID ::
       (MonadIO m, MonadReader WolfServerEnv m)
    => Username
    -> m (Maybe AccountUUID)
lookupAccountUUID un = do
    as <- getAccounts
    pure $ M.lookup un as

-- | Tries to add a new account with the given username.
--
-- If the username already exists, this returns 'Nothing'.
-- If the username does not exist yet, this returns 'Just' with a new 'AccountUUID'.
-- This also adds the new 'AccountUUID' to the global accounts file.
tryToAddNewAccount ::
       (MonadIO m, MonadReader WolfServerEnv m)
    => Username
    -> m (Maybe AccountUUID)
tryToAddNewAccount un = do
    as <- getAccounts
    case M.lookup un as of
        Just _ -> pure Nothing
        Nothing -> do
            uuid <- liftIO newAccountUUID
            storeAccounts $ M.insert un uuid as
            pure $ Just uuid

-- | Retrieve account data
getAccount ::
       (MonadIO m, MonadReader WolfServerEnv m)
    => AccountUUID
    -> m (Maybe Account)
getAccount uuid = do
    adf <- accountDataFile uuid
    readJSONWithMaybe adf

-- | Store account data
--
-- WARNING: this does not store the account in the global accounts file.
storeAccount :: (MonadIO m, MonadReader WolfServerEnv m) => Account -> m ()
storeAccount acc = do
    adf <- accountDataFile $ accountUUID acc
    writeJSON adf acc
