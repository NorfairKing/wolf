{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server where

import Import

import Control.Monad.Except
import Control.Monad.Reader

import Servant.API
import Servant.Server

import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp

import Wolf.Data.Index
import Wolf.Data.Types

import Wolf.API

import Wolf.Server.OptParse
import Wolf.Server.Types

runWolfServer :: IO ()
runWolfServer = do
    (DispatchServe ServeSettings {..}, Settings {..}) <- getInstructions
    let env = WolfServerEnv {wseDataSettings = setDataSettings}
    Warp.run serveSetPort $ wolfApp env

wolfApp :: WolfServerEnv -> Wai.Application
wolfApp = serve wolfAPI . makeWolfServer

makeWolfServer :: WolfServerEnv -> Server WolfAPI
makeWolfServer cfg = enter (readerToEither cfg) wolfServer
  where
    readerToEither :: WolfServerEnv -> WolfHandler :~> ExceptT ServantErr IO
    readerToEither env = Nat $ \x -> runReaderT x env

wolfServer :: ServerT WolfAPI WolfHandler
wolfServer = servePostNewPerson :<|> serveGetPersonEntry

runData :: ReaderT DataSettings IO a -> WolfHandler a
runData func = do
    ds <- asks wseDataSettings
    liftIO $ runReaderT func ds

servePostNewPerson :: PersonEntry -> WolfHandler PersonUuid
servePostNewPerson = undefined

serveGetPersonEntry :: PersonUuid -> WolfHandler PersonEntry
serveGetPersonEntry personUuid = do
    mpe <- runData $ getPersonEntry personUuid
    case mpe of
        Nothing ->
            throwError $
            err404
            { errBody =
                  "Person entry for person with uuid " <>
                  personUuidLBs personUuid <>
                  " not found."
            }
        Just pe -> pure pe
