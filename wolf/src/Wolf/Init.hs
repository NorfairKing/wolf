{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Init where

import Import

import Data.Time

import Wolf.JSONUtils
import Wolf.OptParse.Types
import Wolf.Path
import Wolf.Types

genInitData :: MonadIO m => m InitData
genInitData = liftIO $ InitData <$> getCurrentTime

withInitCheck :: (MonadIO m, MonadReader Settings m) => m () -> m ()
withInitCheck func = do
    iFile <- initFile
    mid <- readJSONWithDefault Nothing iFile
    wd <- wolfDir
    case mid of
        Just InitData {..} -> func
        _ ->
            liftIO $
            die $
            unwords
                [ "No wolf repository has been initialised in"
                , toFilePath wd
                , "yet."
                ]
