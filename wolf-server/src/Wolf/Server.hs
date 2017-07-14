{-# LANGUAGE RecordWildCards #-}

module Wolf.Server where

import Import

import Servant.API
import Servant.Server

import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp

import Wolf.Types

import Wolf.API

-- import Wolf.Server.Types
import Wolf.Server.OptParse

runWolfServer :: IO ()
runWolfServer = do
    (DispatchServe ServeSettings {..}, Settings {..}) <- getInstructions
    Warp.run serveSetPort wolfApp

wolfApp :: Wai.Application
wolfApp = serve wolfAPI wolfServer

wolfServer :: Server WolfAPI
wolfServer = servePostNewPerson :<|> serveGetPersonEntry

servePostNewPerson :: PersonEntry -> Handler PersonUuid
servePostNewPerson = undefined

serveGetPersonEntry :: PersonUuid -> Handler PersonEntry
serveGetPersonEntry = undefined
