{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Alias where

import Import

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data.Index

alias :: (MonadIO m, MonadReader Settings m) => Text -> Text -> m ()
alias new old = do
    origIndex <- runData getIndexWithDefault
    personUuid <-
        case lookupInIndex old origIndex of
            Nothing ->
                liftIO $ die $ unwords ["Reference", show old, "not found."]
            Just puid -> pure puid
    let index = addIndexEntry new personUuid origIndex
    runData $ putIndex index
