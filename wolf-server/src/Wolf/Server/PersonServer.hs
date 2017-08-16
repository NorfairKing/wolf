{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
    serveGetPersonEntry :<|> servePostNewPerson :<|> serveGetPersonByAlias :<|>
    servePostPersonSetAlias :<|>
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

serveGetPersonByAlias :: Text -> WolfHandler PersonUuid
serveGetPersonByAlias key = do
    mPersonUuid <- runData $ (>>= lookupInIndex key) <$> getIndex
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

servePostPersonSetAlias :: SetPersonAlias -> WolfHandler ()
servePostPersonSetAlias SetPersonAlias {..} =
    runData $ do
        index <- getIndexWithDefault
        let index' =
                addIndexEntry setPersonAliasAlias setPersonAliasPersonUuid index
        putIndex index'

serveGetPersonQuery :: PersonQuery -> WolfHandler [PersonUuid]
serveGetPersonQuery = undefined
