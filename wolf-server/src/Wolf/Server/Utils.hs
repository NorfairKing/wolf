module Wolf.Server.Utils where

import Import

import Control.Monad.Reader

import Wolf.Data.Types

import Wolf.Server.Types

runData :: ReaderT DataSettings IO a -> WolfHandler a
runData func = do
    ds <- asks wseDataSettings
    liftIO $ runReaderT func ds
