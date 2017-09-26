{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Web.Server.Foundation
    ( module Wolf.Web.Server.Foundation
    , module Wolf.Web.Server.Widget
    , module Wolf.Web.Server.Constants
    ) where

import Import

import Control.Monad.Except
import Control.Monad.Reader

import Yesod

import Wolf.Data

import Wolf.Server.Path
import Wolf.Server.Types

import Wolf.Web.Server.Constants
import Wolf.Web.Server.Widget

data ServerDataSettings
    = PersonalServer DataSettings
    | SharedServer WolfServerEnv

newtype App = App
    { appDataSettings :: ServerDataSettings
    }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App

runData :: ReaderT DataSettings IO a -> Handler a
runData func = do
    sds <- appDataSettings <$> getYesod
    case sds of
        PersonalServer ds -> liftIO $ runReaderT func ds
        SharedServer wse -> do
            uuid <- error "Not implemented yet: Accounts"
            dd <- runReaderT (accountDataDir uuid) wse
            let ds = DataSettings {dataSetWolfDir = dd}
            liftIO $ runReaderT func ds

instance PathPiece PersonUuid where
    fromPathPiece = parsePersonUuid
    toPathPiece = personUuidText
