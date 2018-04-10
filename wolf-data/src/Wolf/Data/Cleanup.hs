{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Cleanup
    ( cleanupRepo
    ) where

import Import

import Wolf.Data.Export
import Wolf.Data.Import
import Wolf.Data.Types

import Cautious.CautiousT

cleanupRepo ::
       (MonadIO m, MonadReader DataSettings m)
    => (ExportWarning -> m Bool)
    -> m ()
cleanupRepo askToPerformCleanup = do
    mr <- runCautiousT exportRepo
    case mr of
        CautiousError e -> liftIO . die $ prettyShowExportError e
        CautiousWarning [] repo -> importRepo repo
        CautiousWarning w repo -> do
            performCleanup <- askToPerformCleanup w
            when performCleanup $ importRepo repo
