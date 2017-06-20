{-# LANGUAGE FlexibleContexts #-}

module Wolf.Init where

import Import

import Wolf.Git
import Wolf.OptParse.Types
import Wolf.Path

init :: (MonadIO m, MonadReader Settings m) => m ()
init = wolfDir >>= initIn

initIn :: MonadIO m => Path Abs Dir -> m ()
initIn dir = do
    ensureDir dir
    runGitIn dir ["init"]
