{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Export
    ( Export
    , exportInitData
    , export
    ) where

import Import

import Wolf.Data.Export.Types
import Wolf.Data.Init
import Wolf.Data.Types

export :: (MonadIO m, MonadReader DataSettings m) => m (Maybe Export)
export = do
    mid <- readInitData
    case mid of
        Nothing -> pure Nothing
        Just initData -> do
            pure $ Just Export {exportInitData = initData}
