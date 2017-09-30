{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Text.Hamlet
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

instance Yesod App where
    defaultLayout widget = do
        pc <-
            widgetToPageContent $ do
                addStylesheetRemote
                    "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.5.3/css/bulma.min.css"
                --    "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                $(widgetFile "default-body")
        withUrlRenderer $(hamletFile "templates/default-page.hamlet")
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

runDataApp :: App -> ReaderT DataSettings IO a -> IO a
runDataApp app func = do
    let sds = appDataSettings app
    case sds of
        PersonalServer ds -> runReaderT func ds
        SharedServer wse -> do
            uuid <- error "Not implemented yet: Accounts"
            dd <- runReaderT (accountDataDir uuid) wse
            let ds = DataSettings {dataSetWolfDir = dd}
            runReaderT func ds

runData :: ReaderT DataSettings IO a -> Handler a
runData func = do
    app <- getYesod
    liftIO $ runDataApp app func

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance PathPiece PersonUuid where
    fromPathPiece = parsePersonUuid
    toPathPiece = personUuidText

withNavBar :: WidgetT App IO () -> WidgetT App IO ()
withNavBar widget = $(widgetFile "with-nav-bar")

genToken :: MonadHandler m => m Html
genToken = do
    req <- getRequest
    let tokenKey = defaultCsrfParamName
    pure $
        case reqToken req of
            Nothing -> mempty
            Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]
