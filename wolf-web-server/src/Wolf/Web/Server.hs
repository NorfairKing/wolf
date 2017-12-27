{-# LANGUAGE RecordWildCards #-}

module Wolf.Web.Server
    ( wolfWebServer
    , makeWolfApp
    ) where

import Import

import qualified Network.HTTP.Client as Http
import Yesod

import Wolf.Data

import Wolf.Server.Types

import Wolf.Web.Server.Application ()
import Wolf.Web.Server.Foundation
import Wolf.Web.Server.Handler.Git
import Wolf.Web.Server.OptParse

wolfWebServer :: IO ()
wolfWebServer = do
    (DispatchServe ServeSettings {..}, Settings) <- getInstructions
    let wse = WolfServerEnv {wseDataDir = serveSetDataDir}
    app <- makeWolfApp wse
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
