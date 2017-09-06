module Wolf.Google where

import Import

import Control.Monad.Reader

import Wolf.Google.OptParse
import Wolf.Google.Suggest

wolfGoogle :: IO ()
wolfGoogle = do
    (DispatchSuggest ds, Settings) <- getInstructions
    runReaderT suggest ds
