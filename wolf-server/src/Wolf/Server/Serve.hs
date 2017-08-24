{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.Serve where

import Import

import qualified Data.Text.Encoding as TE

import Control.Monad.Except

import Servant
import qualified Servant.Server as Servant (serve)
import Servant.Server

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
    let check (BasicAuthData username password) =
            flip runReaderT se $
            case TE.decodeUtf8' username of
                Left _ -> pure Unauthorized
                Right usernameText -> do
                    muuid <- lookupAccountUUID usernameText
                    case muuid of
                        Nothing -> pure NoSuchUser
                        Just uuid -> do
                            ma <- getAccount uuid
                            case ma of
                                Nothing -> pure NoSuchUser
                                Just acc@Account {..} ->
                                    if validatePassword
                                           accountPasswordHash
                                           password
                                        then pure $ Authorized acc
                                        else pure BadPassword
    in BasicAuthCheck check
