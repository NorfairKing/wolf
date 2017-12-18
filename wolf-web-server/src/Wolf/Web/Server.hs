{-# LANGUAGE RecordWildCards #-}

module Wolf.Web.Server where

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
    (DispatchServe ss@ServeSettings {..}, Settings) <- getInstructions
    man <- Http.newManager Http.defaultManagerSettings
    let sds =
            case serveSetDataSets of
                PersonalSets dd ->
                    PersonalServer DataSettings {dataSetWolfDir = dd}
                SharedSets dd -> SharedServer WolfServerEnv {wseDataDir = dd}
    warp
        serveSetPort
        App
        { appDataSettings = sds
        , appHttpManager = man
        , appGit = WaiSubsite {runWaiSubsite = gitApplication sds} -- TODO find a better way to not duplicate this sds?
        }
