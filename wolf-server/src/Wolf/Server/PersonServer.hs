{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.PersonServer
  ( personServer
  ) where

import Import

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE

import Control.Monad.Except

import Servant.API
import Servant.Server

import Wolf.API
import Wolf.Data

import Wolf.Server.Types
import Wolf.Server.Utils

personServer :: ServerT PersonAPI WolfHandler
personServer =
  serveGetPersonEntry :<|> servePostNewPerson :<|> serveGetPersonByAlias :<|>
  servePostPersonSetAlias :<|>
  serveGetPersonQuery

serveGetPersonEntry :: Account -> PersonUuid -> WolfHandler PersonEntry
serveGetPersonEntry acc personUuid = do
  mpe <- runDataForAccount acc $ getPersonEntry personUuid
  case mpe of
    Nothing ->
      throwError $
      err404
        { errBody =
            "Person entry for person with uuid " <> uuidLBs personUuid <>
            " not found."
        }
    Just pe -> pure pe

servePostNewPerson :: Account -> PersonEntry -> WolfHandler PersonUuid
servePostNewPerson acc pe = do
  personUuid <- liftIO nextRandomUUID
  runDataForAccount acc $ putPersonEntry personUuid pe
  pure personUuid

serveGetPersonByAlias :: Account -> Alias -> WolfHandler PersonUuid
serveGetPersonByAlias acc key = do
  mPersonUuid <- runDataForAccount acc $ (>>= lookupInIndex key) <$> getIndex
  case mPersonUuid of
    Nothing ->
      throwError $
      err404
        { errBody =
            "Person uuid for person with alias " <>
            LB.fromStrict (TE.encodeUtf8 $ aliasText key) <>
            " not found."
        }
    Just personUuid -> pure personUuid

servePostPersonSetAlias :: Account -> SetPersonAlias -> WolfHandler ()
servePostPersonSetAlias acc SetPersonAlias {..} =
  runDataForAccount acc $ do
    index <- getIndexWithDefault
    let index' =
          addIndexEntry setPersonAliasAlias setPersonAliasPersonUuid index
    putIndex index'

serveGetPersonQuery :: Account -> PersonQuery -> WolfHandler [PersonUuid]
serveGetPersonQuery _ = undefined
