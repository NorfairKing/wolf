module Wolf.Google.Suggest where

import Import

import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy.Char8 as LB8
import Lens.Micro

import Network.Google.People

import Wolf.Google.Suggest.Call
import Wolf.Google.Suggest.Gather

suggest :: IO ()
suggest = do
    resp <- getPeople
    forM_ (resp ^. lcrConnections) $ \p -> do
        LB8.putStrLn $ JSON.encodePretty p
        LB8.putStrLn $ JSON.encodePretty $ gatherData p
