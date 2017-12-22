module Wolf.Server
    ( runWolfServer
    ) where

import Import

import Wolf.Server.OptParse
import Wolf.Server.Serve

runWolfServer :: IO ()
runWolfServer = do
    (DispatchServe ss, sets) <- getInstructions
    serve ss sets
