{-# LANGUAGE RecordWildCards #-}

module Wolf.Web.Server
    ( wolfWebServer
    , makeWolfApp
    ) where

import Import

import Control.Concurrent.Async (concurrently_)
import qualified Network.HTTP.Client as Http
import qualified Network.Wai.Handler.Warp as Warp
import Yesod

import qualified Wolf.Server.Serve as API
import Wolf.Server.Types

import Wolf.Web.Server.Application ()
import Wolf.Web.Server.Foundation
import Wolf.Web.Server.Handler.Git
import Wolf.Web.Server.OptParse

wolfWebServer :: IO ()
wolfWebServer = do
    (DispatchServe ss, Settings) <- getInstructions
    concurrently_ (runWolfWebServer ss) (runWolfAPIServer ss)

makeWolfServerEnv :: ServeSettings -> WolfServerEnv
makeWolfServerEnv ServeSettings {..} =
    WolfServerEnv {wseDataDir = serveSetDataDir}

runWolfWebServer :: ServeSettings -> IO ()
runWolfWebServer ss@ServeSettings {..} = do
    app <- makeWolfApp $ makeWolfServerEnv ss
    warp serveSetPort app

makeWolfApp :: WolfServerEnv -> IO App
makeWolfApp wse = do
    man <- Http.newManager Http.defaultManagerSettings
    pure
        App
            { appDataSettings = wse
            , appHttpManager = man
            , appStatic = myStatic
            , appGit = WaiSubsite {runWaiSubsite = gitApplication wse} -- TODO find a better way to not duplicate this sds?
            }

runWolfAPIServer :: ServeSettings -> IO ()
runWolfAPIServer ss@ServeSettings {..} =
    case serveSetAPIPort of
        Nothing -> pure ()
        Just p -> do
            let app = API.wolfApp $ makeWolfServerEnv ss
            Warp.run p app
