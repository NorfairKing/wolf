{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.PersonServer
    ( personServer
    ) where

import Import

import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Control.Monad.Except

import Servant.API
import Servant.Server

import Wolf.Data.Entry.Types
import Wolf.Data.Index
import Wolf.Data.Types

import Wolf.API

import Wolf.Server.Types
import Wolf.Server.Utils

personServer :: ServerT WolfAPI WolfHandler
personServer =
    serveGetPersonEntry :<|> servePostNewPerson :<|> serveGetPerson :<|>
    serveGetPersonQuery

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
servePostNewPerson pe = do
    personUuid <- liftIO nextRandomPersonUuid
    runData $ putPersonEntry personUuid pe
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
