{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Cleanup
    ( cleanup
    ) where

import Import

import Wolf.Data

import Wolf.Cli.OptParse
import Wolf.Cli.Utils

cleanup :: (MonadIO m, MonadReader Settings m) => m ()
cleanup = runData $ withInitCheck_ cleanupRepo
