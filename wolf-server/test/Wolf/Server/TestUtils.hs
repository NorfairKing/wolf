{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Wolf.Server.TestUtils
    ( withWolfServer
    , runClient
    , runClientOrError
    ) where

import TestImport

import qualified Network.HTTP.Client as HTTP

import Servant
import Servant.Client

import Network.Wai.Handler.Warp (withApplication)

import Wolf.API
import Wolf.Data.Types
import Wolf.Server.Serve
import Wolf.Server.Types

withWolfServer :: SpecWith ClientEnv -> Spec
withWolfServer specFunc = do
    let getDataDir :: IO (Path Abs Dir)
        getDataDir = resolveDir' "test-sandbox"
    let setupDSAndMan :: IO (DataSettings, HTTP.Manager)
        setupDSAndMan = do
            wd <- resolveDir' "test-sandbox"
            let ds = DataSettings {dataSetWolfDir = wd}
            man <- HTTP.newManager HTTP.defaultManagerSettings
            pure (ds, man)
    let withApp :: (ClientEnv -> IO ()) -> (DataSettings, HTTP.Manager) -> IO ()
        withApp func (ds, man) = do
            let wolfEnv = WolfServerEnv {wseDataSettings = ds}
            let getServer = pure $ makeWolfServer wolfEnv
            withServantServer wolfAPI getServer $ \burl ->
                let cenv = ClientEnv man burl
                in func cenv
    let cleanup = do
            wd <- getDataDir -- FIXME could go wrong if the server makes any symbolic links
            ignoringAbsence $ removeDirRecur wd
    afterAll_ cleanup $ beforeAll setupDSAndMan $ aroundWith withApp specFunc

-- | Start a servant application on an open port, run the provided function,
-- then stop the application.
withServantServer ::
       HasServer a '[] => Proxy a -> IO (Server a) -> (BaseUrl -> IO r) -> IO r
withServantServer api = withServantServerAndContext api EmptyContext

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
