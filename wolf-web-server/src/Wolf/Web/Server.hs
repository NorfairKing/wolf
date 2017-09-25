module Wolf.Web.Server where

import Import

import Yesod

import Wolf.Web.Server.Application ()
import Wolf.Web.Server.Foundation
import Wolf.Web.Server.OptParse

wolfWebServer :: IO ()
wolfWebServer = do
    (DispatchServe, Settings) <- getInstructions
    warp 3000 App
