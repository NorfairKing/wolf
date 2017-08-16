{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.Serve where

import Import

import Control.Monad.Except
import Control.Monad.Reader

import qualified Servant.Server as Servant (serve)
import Servant.Server

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Wolf.API

import Wolf.Server.OptParse
import Wolf.Server.PersonServer
import Wolf.Server.Types

serve :: ServeSettings -> Settings -> IO ()
serve ServeSettings {..} Settings {..} = do
    let env = WolfServerEnv {wseDataSettings = setDataSettings}
    Warp.run serveSetPort $ wolfApp env

wolfApp :: WolfServerEnv -> Wai.Application
wolfApp = Servant.serve wolfAPI . makeWolfServer

makeWolfServer :: WolfServerEnv -> Server WolfAPI
makeWolfServer cfg = enter (readerToEither cfg) wolfServer
  where
    readerToEither :: WolfServerEnv -> WolfHandler :~> ExceptT ServantErr IO
    readerToEither env = Nat $ \x -> runReaderT x env

wolfServer :: ServerT WolfAPI WolfHandler
wolfServer = personServer
