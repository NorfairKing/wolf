{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.AccountServer
    ( accountServer
    ) where

import Import

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE

import Control.Monad.Except

import Servant.API
import Servant.Server

import Wolf.Data.Entry.Types
import Wolf.Data.Index
import Wolf.Data.Types

import Wolf.API

import Wolf.Server.Accounts
import Wolf.Server.Types
import Wolf.Server.Utils

accountServer :: ServerT AccountAPI WolfHandler
accountServer = servePostRegister

servePostRegister :: Register -> WolfHandler AccountUUID
servePostRegister Register {..} = do
    mh <- liftIO $ hashPassword registerPassword
    case mh of
        Nothing -> throwError $ err400 {errBody = "Failed to hash password."}
        Just ph -> do
            muuid <- tryToAddNewAccount registerUsername
            case muuid of
                Nothing ->
                    throwError $
                    err400
                    {errBody = "Account with this username already exists."}
                Just uuid -> do
                    let acc =
                            Account
                            { accountUUID = uuid
                            , accountUsername = registerUsername
                            , accountPasswordHash = ph
                            }
                    storeAccount acc
                    pure uuid
