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

spec :: Spec
spec =
    describe "wolf server" $
    it "starts up nicely" $
    withServantServer wolfAPI (pure wolfServer) $ const $ pure ()

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
