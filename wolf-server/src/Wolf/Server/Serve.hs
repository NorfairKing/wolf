{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.Serve where

import Import

import Servant

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Wolf.API

import Wolf.Server.AccountServer
import Wolf.Server.Auth
import Wolf.Server.OptParse
import Wolf.Server.PersonServer
import Wolf.Server.Types

serve :: ServeSettings -> Settings -> IO ()
serve ServeSettings {..} Settings = do
    let env = WolfServerEnv {wseDataDir = serveSetDataDir}
    Warp.run serveSetPort $ wolfApp env

wolfApp :: WolfServerEnv -> Wai.Application
wolfApp se =
    Servant.serveWithContext wolfAPI (authContext se) (makeWolfServer se)

makeWolfServer :: WolfServerEnv -> Server WolfAPI
makeWolfServer cfg =
    hoistServerWithContext
        wolfAPI
        (Proxy :: Proxy '[ BasicAuthCheck Account])
        (`runReaderT` cfg)
        wolfServer

wolfServer :: ServerT WolfAPI WolfHandler
wolfServer = accountServer :<|> personServer
