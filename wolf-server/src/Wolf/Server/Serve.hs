{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.Serve where

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

serve :: ServeSettings -> Settings -> IO ()
serve ServeSettings {..} Settings = do
    let env = WolfServerEnv {wseDataDir = serveSetDataDir}
    Warp.run serveSetPort $ wolfApp env

wolfApp :: WolfServerEnv -> Wai.Application
wolfApp se =
    Servant.serveWithContext wolfAPI (authContext se) (makeWolfServer se)

makeWolfServer :: WolfServerEnv -> Server WolfAPI
makeWolfServer cfg = enter (readerToEither cfg) wolfServer
  where
    readerToEither :: WolfServerEnv -> WolfHandler :~> ExceptT ServantErr IO
    readerToEither env = Nat $ \x -> runReaderT x env

wolfServer :: ServerT WolfAPI WolfHandler
wolfServer = accountServer :<|> personServer

authContext :: WolfServerEnv -> Context (BasicAuthCheck Account ': '[])
authContext se = authCheck se :. EmptyContext

authCheck :: WolfServerEnv -> BasicAuthCheck Account
authCheck se =
    BasicAuthCheck $ \(BasicAuthData username password) ->
        flip runReaderT se $
        short (either (const Nothing) Just $ TE.decodeUtf8' username) $ \usernameText -> do
            muuid <- lookupAccountUUID usernameText
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
