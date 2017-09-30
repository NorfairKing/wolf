{-# LANGUAGE FlexibleContexts #-}

module Wolf.Web.Server.TestUtils where

import TestImport

import Yesod.Test

import Control.Monad.Reader

import Wolf.Data
import Wolf.Data.Gen ()

import Wolf.Web.Server.Application ()
import Wolf.Web.Server.Foundation

-- TODO Also test the shared server automatically
wolfWebServerSpec :: YesodSpec App -> Spec
wolfWebServerSpec =
    yesodSpecWithSiteGenerator $ do
        dd <- resolveDir' "/tmp/wolf-web-server-test"
        ignoringAbsence $ removeDirRecur dd
        let ds = DataSettings {dataSetWolfDir = dd}
        runReaderT setupTestData ds
        let sds = PersonalServer ds
        pure App {appDataSettings = sds}

setupTestData :: ReaderT DataSettings IO ()
setupTestData = do
    repo <- liftIO $ generate genValid
    importRepo repo

runTestData :: ReaderT DataSettings IO a -> YesodExample App a
runTestData func = do
    app_ <- getTestYesod
    liftIO $ runDataApp app_ func
