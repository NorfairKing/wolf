{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Web.Server.TestUtils where

import TestImport

import Yesod.Test

import Control.Monad.Reader
import qualified Data.Text as T
import Data.Text (Text)

import Network.HTTP.Types

import Wolf.API
import Wolf.Data

import Wolf.Server.Accounts
import Wolf.Server.Path
import Wolf.Server.Types

import Wolf.Web.Server
import Wolf.Web.Server.Foundation

import Wolf.Data.Gen ()

testSandbox :: IO (Path Abs Dir)
testSandbox = resolveDir' "/tmp/wolf-web-server-test"

wolfTestServerEnv :: IO WolfServerEnv
wolfTestServerEnv = WolfServerEnv <$> testSandbox

wolfWebServerPersonalSpec :: YesodSpec App -> Spec
wolfWebServerPersonalSpec =
    before_
        (do sb <- wseDataDir <$> wolfTestServerEnv
            ignoringAbsence $ removeDirRecur sb) .
    yesodSpecWithSiteGenerator
        (do env <- wolfTestServerEnv
            let clear = ignoringAbsence $ removeDirRecur $ wseDataDir env
            clear
            makeWolfApp env)

setupTestData :: ReaderT DataSettings IO ()
setupTestData = do
    repo <- liftIO $ generate genValid
    importRepo repo

runTestServer :: ReaderT WolfServerEnv IO a -> YesodExample App a
runTestServer func = liftIO $ wolfTestServerEnv >>= runReaderT func

runTestDataShared ::
       AccountUUID -> ReaderT DataSettings IO a -> YesodExample App a
runTestDataShared uuid func = do
    wse <- liftIO wolfTestServerEnv
    ad <- runReaderT (accountDataDir uuid) wse
    let ds = DataSettings {dataSetWolfDir = ad}
    liftIO $ runReaderT func ds

withFreshAccount ::
       Username
    -> Text
    -> (AccountUUID -> YesodExample App a)
    -> YesodExample App a
withFreshAccount exampleUsername examplePassphrase func = do
    get $ AuthR registerR
    statusIs 200
    request $ do
        setMethod methodPost
        setUrl $ AuthR registerR
        addTokenFromCookie
        addPostParam "username" $ usernameText exampleUsername
        addPostParam "passphrase" examplePassphrase
        addPostParam "passphrase-confirm" examplePassphrase
    statusIs 303
    loc <- followRedirect
    liftIO $ loc `shouldBe` Right "/"
    muuid <- runTestServer $ lookupAccountUUID exampleUsername
    case muuid of
        Nothing -> do
            liftIO $
                expectationFailure
                    "expected to have a user with the example username."
            undefined -- Fine, won't get here anyway
        Just uuid -> func uuid

withExampleAccount :: (AccountUUID -> YesodExample App a) -> YesodExample App a
withExampleAccount = withFreshAccount (fromJust $ username "example") "pass"

withExampleAccount_ :: YesodExample App a -> YesodExample App a
withExampleAccount_ = withExampleAccount . const
