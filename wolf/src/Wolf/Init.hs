{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Init where

import Import

import Data.Time

import Wolf.Git
import Wolf.JSONUtils
import Wolf.OptParse.Types
import Wolf.Path
import Wolf.Types

init :: (MonadIO m, MonadReader Settings m) => m ()
init = do
    dir <- wolfDir
    iFile <- initFile
    mex <- liftIO $ forgivingAbsence $ doesFileExist iFile
    case mex of
        Just True -> do
            d <- readJSON iFile
            liftIO $
                die $
                unwords
                    [ "A wolf repository has already been initialised in"
                    , toFilePath dir
                    , "on"
                    , show $ initTimestamp d
                    ]
        _ -> do
            ensureDir dir
            d <- genInitData
            writeJSON iFile d
            gitInit
            makeGitCommit "Initial commit"

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
