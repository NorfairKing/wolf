{-# LANGUAGE FlexibleContexts #-}

module Wolf.Alias where

import Import

import Wolf.Index
import Wolf.OptParse.Types

alias :: (MonadIO m, MonadReader Settings m) => String -> String -> m ()
alias new old = do
    origIndex <- getIndex
    personUuid <-
        case lookupInIndex old origIndex of
            Nothing ->
                liftIO $ die $ unwords ["Reference", show old, "not found."]
            Just puid -> pure puid
    let index = addIndexEntry new personUuid origIndex
    putIndex index
