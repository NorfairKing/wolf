{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Cleanup
    ( cleanupRepo
    ) where

import Import

import Wolf.Data.Export
import Wolf.Data.Import
import Wolf.Data.Types

cleanupRepo :: (MonadIO m, MonadReader DataSettings m) => m ()
cleanupRepo = do
    mr <- exportRepo
    case mr of
        Nothing -> pure ()
        Just r -> importRepo r
