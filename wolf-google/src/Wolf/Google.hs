module Wolf.Google where

import Import

import Control.Monad.Reader

import Wolf.Google.Suggest
import Wolf.Google.OptParse

wolfGoogle :: IO ()
wolfGoogle = do
    (DispatchSuggest ds, Settings) <- getInstructions
    runReaderT suggest ds
