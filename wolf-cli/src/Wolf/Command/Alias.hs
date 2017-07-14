{-# LANGUAGE FlexibleContexts #-}

module Wolf.Command.Alias where

import Import

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Index

alias :: (MonadIO m, MonadReader Settings m) => String -> String -> m ()
alias new old = do
    origIndex <- runData getIndex
    personUuid <-
        case lookupInIndex old origIndex of
            Nothing ->
                liftIO $ die $ unwords ["Reference", show old, "not found."]
            Just puid -> pure puid
    let index = addIndexEntry new personUuid origIndex
    runData $ putIndex index
