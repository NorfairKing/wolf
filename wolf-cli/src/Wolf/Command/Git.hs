{-# LANGUAGE FlexibleContexts #-}

module Wolf.Command.Git where

import Import

import Wolf.Cli.Utils
import Wolf.Git
import Wolf.Init
import Wolf.OptParse.Types

git :: (MonadReader Settings m, MonadIO m) => [String] -> m ()
git = runData . withInitCheck . runGit
