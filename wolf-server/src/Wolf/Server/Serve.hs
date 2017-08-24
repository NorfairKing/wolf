{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.Serve where

import Import

import Control.Monad.Except
import Control.Monad.Reader

import Servant
import qualified Servant.Server as Servant (serve)
import Servant.Server

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Wolf.API

import Wolf.Server.AccountServer
import Wolf.Server.OptParse
import Wolf.Server.PersonServer
import Wolf.Server.Types

serve :: ServeSettings -> Settings -> IO ()
serve ServeSettings {..} Settings = do
    let env = WolfServerEnv {wseDataDir = serveSetDataDir}
    Warp.run serveSetPort $ wolfApp env

wolfApp :: WolfServerEnv -> Wai.Application
wolfApp = Servant.serveWithContext wolfAPI authContext . makeWolfServer

makeWolfServer :: WolfServerEnv -> Server WolfAPI
makeWolfServer cfg = enter (readerToEither cfg) wolfServer
  where
    readerToEither :: WolfServerEnv -> WolfHandler :~> ExceptT ServantErr IO
    readerToEither env = Nat $ \x -> runReaderT x env

wolfServer :: ServerT WolfAPI WolfHandler
wolfServer = accountServer :<|> personServer

authContext :: Context (BasicAuthCheck Account ': '[])
authContext = authCheck :. EmptyContext

authCheck :: BasicAuthCheck Account
authCheck =
    let check (BasicAuthData username password) = undefined
    in BasicAuthCheck check
