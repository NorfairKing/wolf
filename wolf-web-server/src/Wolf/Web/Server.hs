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
    let sds =
            case serveSetDataSets of
                PersonalSets dd ->
                    PersonalServer DataSettings {dataSetWolfDir = dd}
                SharedSets dd -> SharedServer WolfServerEnv {wseDataDir = dd}
    app <- makeWolfApp sds
    warp serveSetPort app

makeWolfApp :: ServerDataSettings -> IO App
makeWolfApp sds = do
    man <- Http.newManager Http.defaultManagerSettings
    pure
        App
        { appDataSettings = sds
        , appHttpManager = man
        , appGit = WaiSubsite {runWaiSubsite = gitApplication sds} -- TODO find a better way to not duplicate this sds?
        }
