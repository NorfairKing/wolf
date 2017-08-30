{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Utils where

import Import

import Wolf.Cli.OptParse.Types
import Wolf.Data

runData :: (MonadReader Settings m) => ReaderT DataSettings m a -> m a
runData func = do
    dataSets <- asks setDataSets
    runReaderT func dataSets
