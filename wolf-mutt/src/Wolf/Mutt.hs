module Wolf.Mutt
    ( wolfMutt
    ) where

import Import

import Control.Monad.Reader

import Wolf.Mutt.OptParse
import Wolf.Mutt.Query

wolfMutt :: IO ()
wolfMutt = do
    Instructions disp sets <- getInstructions
    flip runReaderT sets $
        case disp of
            DispatchQuery q -> wolfMuttQuery q
