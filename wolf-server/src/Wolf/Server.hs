{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server where

import Import

import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Control.Monad.Except
import Control.Monad.Reader

import Servant.API
import Servant.Server

import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp

import Wolf.Data.Entry.Types
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
wolfServer =
    serveGetPersonEntry :<|> servePostNewPerson :<|> serveGetPerson :<|>
    serveGetPersonQuery

runData :: ReaderT DataSettings IO a -> WolfHandler a
runData func = do
    ds <- asks wseDataSettings
    liftIO $ runReaderT func ds

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

servePostNewPerson :: PersonEntry -> WolfHandler PersonUuid
servePostNewPerson personEntry = do
    personUuid <- liftIO nextRandomPersonUuid
    runData $ putPersonEntry personUuid personEntry
    pure personUuid

serveGetPerson :: Text -> WolfHandler PersonUuid
serveGetPerson key = do
    mPersonUuid <- runData $ lookupInIndex key <$> getIndexWithDefault
    case mPersonUuid of
        Nothing ->
            throwError $
            err404
            { errBody =
                  "Person uuid for person with key " <>
                  LB.fromStrict (TE.encodeUtf8 key) <>
                  " not found."
            }
        Just personUuid -> pure personUuid

serveGetPersonQuery :: PersonQuery -> WolfHandler [PersonUuid]
serveGetPersonQuery = undefined
