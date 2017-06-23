{-# LANGUAGE FlexibleContexts #-}

module Wolf.Command.Init where

import Import

import Wolf.Git
import Wolf.Init
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
