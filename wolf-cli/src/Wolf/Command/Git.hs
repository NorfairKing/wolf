{-# LANGUAGE FlexibleContexts #-}

module Wolf.Command.Git where

import Import

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data.Git
import Wolf.Data.Init

git :: (MonadReader Settings m, MonadIO m) => [String] -> m ()
git = runData . withInitCheck . runGit
