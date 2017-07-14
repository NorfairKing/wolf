{-# LANGUAGE FlexibleContexts #-}

module Wolf.Command.Init where

import Import

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Git
import Wolf.Init
import Wolf.JSONUtils
import Wolf.Path
import Wolf.Types

init :: (MonadIO m, MonadReader Settings m) => m ()
init = do
    dir <- runData wolfDir
    iFile <- runData initFile
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
            runData $ do
                gitInit
                makeGitCommit "Initial commit"
