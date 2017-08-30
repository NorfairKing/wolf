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
import Wolf.Server.Serve
import Wolf.Server.Types

import Wolf.API.Gen ()

testSandbox :: MonadIO m => m (Path Abs Dir)
testSandbox = liftIO $ resolveDir' "test-sandbox"

withEnv :: WolfServerEnv -> ReaderT WolfServerEnv IO a -> IO a
withEnv = flip runReaderT

withTestSandbox :: SpecWith WolfServerEnv -> Spec
withTestSandbox = around withSandbox
  where
    withSandbox :: ActionWith WolfServerEnv -> IO ()
    withSandbox func = do
        sb <- testSandbox
        let clear = ignoringAbsence $ removeDirRecur sb
        clear
        let env = WolfServerEnv sb
        func env
        clear

withWolfServer :: SpecWith ClientEnv -> Spec
withWolfServer specFunc = do
    let setupMan :: IO HTTP.Manager
        setupMan = HTTP.newManager HTTP.defaultManagerSettings
    let withApp :: (ClientEnv -> IO ()) -> HTTP.Manager -> IO ()
        withApp func man = do
            dd <- testSandbox
            let clear = ignoringAbsence $ removeDirRecur dd
            clear
            let wolfEnv = WolfServerEnv {wseDataDir = dd}
            let getServer = pure $ makeWolfServer wolfEnv
            withServantServerAndContext wolfAPI (authContext wolfEnv) getServer $ \burl ->
                let cenv = ClientEnv man burl
                in func cenv
            clear
    beforeAll setupMan $ aroundWith withApp specFunc

-- | Like 'withServantServer', but allows passing in a 'Context' to the
-- application.
withServantServerAndContext ::
       HasServer a ctx
    => Proxy a
    -> Context ctx
    -> IO (Server a)
    -> (BaseUrl -> IO r)
    -> IO r
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
    forAll genValid $ \register -> do
        errOrUuid <- runClient cenv $ clientPostRegister register
        case errOrUuid of
            Left err ->
                let snf =
                        expectationFailure $
                        "Registration should not fail with error: " <> show err
                in case err of
                       FailureResponse {} ->
                           if statusCode (responseStatus err) == 409
                               then pure () -- Username already exists, just stop here then.
                               else snf
                       _ -> snf
            Right _ -> do
                let basicAuthData =
                        BasicAuthData
                        { basicAuthUsername =
                              TE.encodeUtf8 $
                              usernameText $ registerUsername register
                        , basicAuthPassword =
                              TE.encodeUtf8 $ registerPassword register
                        }
                func basicAuthData
