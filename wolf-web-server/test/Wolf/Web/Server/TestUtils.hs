{-# LANGUAGE FlexibleContexts #-}
module Wolf.Web.Server.TestUtils where

import TestImport

import Yesod.Test

import Control.Monad.Reader
import Control.Monad.State

import Wolf.Data

import Wolf.Web.Server.Application
import Wolf.Web.Server.Foundation

-- TODO Also test the shared server automatically
wolfWebServerSpec :: YesodSpec App -> Spec
wolfWebServerSpec =
    yesodSpecWithSiteGenerator $ do
        dd <- resolveDir' "/tmp/wolf-web-server-test"
        let sds = PersonalServer DataSettings {dataSetWolfDir = dd}
        pure $ App {appDataSettings = sds}

runTestData ::
       (MonadIO m, MonadState (YesodExampleData App) m)
    => ReaderT DataSettings IO a
    -> m a
runTestData func = do
    app <- gets yedSite
    liftIO $ runDataApp app func
