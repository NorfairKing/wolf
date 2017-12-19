{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.Auth
    ( authContext
    , authCheck
    , basicAuthCheck
    , BasicAuthResult(..)
    ) where

import Import

import qualified Data.Text.Encoding as TE

import Control.Monad.Except

import Servant

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Wolf.API

import Wolf.Server.AccountServer
import Wolf.Server.Accounts
import Wolf.Server.OptParse
import Wolf.Server.PersonServer
import Wolf.Server.Types

authContext :: WolfServerEnv -> Context (BasicAuthCheck Account ': '[])
authContext se = authCheck se :. EmptyContext

authCheck :: WolfServerEnv -> BasicAuthCheck Account
authCheck se =
    BasicAuthCheck $ \(BasicAuthData usernameBs password) ->
        basicAuthCheck se usernameBs password

basicAuthCheck ::
       WolfServerEnv -> ByteString -> ByteString -> IO (BasicAuthResult Account)
basicAuthCheck se usernameBs password =
    flip runReaderT se $
    short (either (const Nothing) Just $ TE.decodeUtf8' usernameBs) $ \usernameText ->
        short (username usernameText) $ \un -> do
            muuid <- lookupAccountUUID un
            short muuid $ \uuid -> do
                ma <- getAccount uuid
                short ma $ \acc@Account {..} ->
                    pure $
                    if validatePassword accountPasswordHash password
                        then Authorized acc
                        else BadPassword
  where
    short mt func =
        case mt of
            Nothing -> pure NoSuchUser
            Just a -> func a
