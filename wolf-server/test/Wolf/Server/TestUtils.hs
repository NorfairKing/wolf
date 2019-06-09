{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Wolf.Server.TestUtils
  ( withEnv
  , withTestSandbox
  , withWolfServer
  , runClient
  , runClientOrError
  , withValidNewUser
  ) where

import TestImport

import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types

import Servant
import Servant.Client

import Network.Wai.Handler.Warp (withApplication)

import Wolf.API
import Wolf.Client

import Wolf.Server.Auth
import Wolf.Server.Serve
import Wolf.Server.Types

import Wolf.API.Gen ()

testSandbox :: MonadIO m => m (Path Abs Dir)
testSandbox = liftIO $ resolveDir' "/tmp/test-sandbox"

testServerEnv :: MonadIO m => m WolfServerEnv
testServerEnv = WolfServerEnv <$> testSandbox <*>pure Nothing

withEnv :: WolfServerEnv -> ReaderT WolfServerEnv IO a -> IO a
withEnv = flip runReaderT

withTestSandbox :: SpecWith WolfServerEnv -> Spec
withTestSandbox = around withSandbox
  where
    withSandbox :: ActionWith WolfServerEnv -> IO ()
    withSandbox func = do
      env <- testServerEnv
      ignoringAbsence $ removeDirRecur $ wseDataDir env
      func env

withWolfServer :: SpecWith ClientEnv -> Spec
withWolfServer specFunc = do
  let setupMan :: IO HTTP.Manager
      setupMan = HTTP.newManager HTTP.defaultManagerSettings
  let withApp :: (ClientEnv -> IO ()) -> HTTP.Manager -> IO ()
      withApp func man = do
        env <- testServerEnv
        ignoringAbsence $ removeDirRecur $ wseDataDir env
        let getServer = pure $ makeWolfServer env
        withServantServerAndContext wolfAPI (authContext env) getServer $ \burl ->
          func $ ClientEnv man burl Nothing
  beforeAll setupMan $ aroundWith withApp specFunc

-- | Like 'withServantServer', but allows passing in a 'Context' to the
-- application.
withServantServerAndContext ::
     HasServer a ctx => Proxy a -> Context ctx -> IO (Server a) -> (BaseUrl -> IO r) -> IO r
withServantServerAndContext api ctx server t =
  withApplication (serveWithContext api ctx <$> server) $ \port ->
    t (BaseUrl Http "localhost" port "")

runClient :: ClientEnv -> ClientM a -> IO (Either ServantError a)
runClient = flip runClientM

runClientOrError :: ClientEnv -> ClientM a -> IO a
runClientOrError cenv func = do
  errOrRes <- runClient cenv func
  case errOrRes of
    Left err -> do
      expectationFailure $ show err
      undefined -- Won't get here anyway ^
    Right res -> pure res

withValidNewUser :: ClientEnv -> (BasicAuthData -> IO ()) -> Property
withValidNewUser cenv func =
  forAllValid $ \register -> do
    errOrUuid <- runClient cenv $ clientPostRegister register
    case errOrUuid of
      Left err ->
        let snf = expectationFailure $ "Registration should not fail with error: " <> show err
         in case err of
              FailureResponse r ->
                if statusCode (responseStatusCode r) == 409
                  then pure () -- Username already exists, just stop here then.
                  else snf
              _ -> snf
      Right _ -> do
        let basicAuthData =
              BasicAuthData
                { basicAuthUsername = TE.encodeUtf8 $ usernameText $ registerUsername register
                , basicAuthPassword = TE.encodeUtf8 $ registerPassword register
                }
        func basicAuthData
