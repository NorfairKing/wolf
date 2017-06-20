module Wolf.Init where

import Import

import Wolf.Path
import Wolf.Git

init :: MonadIO m => m ()
init = wolfDir >>= initIn

initIn :: MonadIO m => Path Abs Dir -> m ()
initIn dir = do
    ensureDir dir
    gitInit
