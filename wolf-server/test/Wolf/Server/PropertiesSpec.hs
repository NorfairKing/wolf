{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Server.PropertiesSpec
    ( spec
    ) where

import TestImport

import Network.Wai.Handler.Warp (withApplication)

import Servant
import Servant.Client (BaseUrl(..), Scheme(..))

import Wolf.API
import Wolf.Server

import Wolf.Data.Types
import Wolf.Server.Types

spec :: Spec
spec =
    describe "wolf server" $
    it "starts up nicely" $ do
        let getServer = do
                td <- resolveDir' "/tmp/wolf-test-sandbox"
                let env =
                        WolfServerEnv
                        {wseDataSettings = DataSettings {dataSetWolfDir = td}}
                pure $ makeWolfServer env
        withServantServer wolfAPI getServer $ const $ pure ()

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
