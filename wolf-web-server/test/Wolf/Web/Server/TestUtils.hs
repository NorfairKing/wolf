{-# LANGUAGE FlexibleContexts #-}

module Wolf.Web.Server.TestUtils where

import TestImport

import Yesod.Test

import Control.Monad.Reader

import Wolf.API
import Wolf.Data

import Wolf.Server.Accounts
import Wolf.Server.Path
import Wolf.Server.Types

import Wolf.Web.Server
import Wolf.Web.Server.Foundation

import Wolf.Data.Gen ()

wolfWebServerPersonalSpec :: YesodSpec App -> Spec
wolfWebServerPersonalSpec =
    before_
        (do sb <- testSandbox
            ignoringAbsence $ removeDirRecur sb) .
    yesodSpecWithSiteGenerator
        (do dd <- testSandbox
            let ds = DataSettings {dataSetWolfDir = dd}
            runReaderT setupTestData ds
            setupWolfGit "test" dd
            let sds = PersonalServer ds
            makeWolfApp sds)

setupTestData :: ReaderT DataSettings IO ()
setupTestData = do
    repo <- liftIO $ generate genValid
    importRepo repo

runTestDataPersonal :: ReaderT DataSettings IO a -> YesodExample App a
runTestDataPersonal func = do
    dd <- liftIO testSandbox
    liftIO $ runReaderT func DataSettings {dataSetWolfDir = dd}

runTestDataShared ::
       AccountUUID -> ReaderT DataSettings IO a -> YesodExample App a
runTestDataShared uuid func = do
    dd <- liftIO testSandbox
    let wse = WolfServerEnv {wseDataDir = dd}
    ad <- runReaderT (accountDataDir uuid) wse
    let ds = DataSettings {dataSetWolfDir = ad}
    liftIO $ runReaderT func ds

testSandbox :: IO (Path Abs Dir)
testSandbox = resolveDir' "/tmp/wolf-web-server-test"
