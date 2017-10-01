{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Alias where

import Import

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data

alias :: (MonadIO m, MonadReader Settings m) => Alias -> Alias -> m ()
alias new old =
    runData $
    withInitCheck_ $ do
        origIndex <- getIndexWithDefault
        personUuid <-
            case lookupInIndex old origIndex of
                Nothing ->
                    liftIO $ die $ unwords ["Reference", show old, "not found."]
                Just puid -> pure puid
        let index = addIndexEntry new personUuid origIndex
        putIndex index
