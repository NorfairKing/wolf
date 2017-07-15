{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Init where

import Import

import Data.Time

import Wolf.Data.JSONUtils
import Wolf.Data.Path
import Wolf.Data.Types

genInitData :: MonadIO m => m InitData
genInitData = liftIO $ InitData <$> getCurrentTime

withInitCheck :: (MonadIO m, MonadReader DataSettings m) => m () -> m ()
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
