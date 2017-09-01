module Wolf.Google where

import Import

import Wolf.Google.Suggest

import Wolf.Google.OptParse

wolfGoogle :: IO ()
wolfGoogle = do
    (DispatchSuggest, Settings) <- getInstructions
    suggest
