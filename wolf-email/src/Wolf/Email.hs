module Wolf.Email
    ( wolfEmail
    ) where

import Import

import Control.Monad.Reader

import Wolf.Email.OptParse
import Wolf.Email.Suggest

wolfEmail :: IO ()
wolfEmail = do
    Instructions disp sets <- getInstructions
    flip runReaderT sets $
        case disp of
            DispatchSuggest -> wolfEmailSuggest
