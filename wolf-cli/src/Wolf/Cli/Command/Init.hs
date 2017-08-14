{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Init where

import Import

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data.Git
import Wolf.Data.Init
import Wolf.Data.JSONUtils
import Wolf.Data.Path
import Wolf.Data.Types

init :: (MonadIO m, MonadReader Settings m) => m ()
init = do
    dir <- runData wolfDir
    iFile <- runData initFile
    mex <- readJSONWithMaybe iFile
    case mex of
        Just d ->
            liftIO $
            die $
            unwords
                [ "A wolf repository has already been initialised in"
                , toFilePath dir
                , "on"
                , show $ initTimestamp d
                ]
        Nothing -> do
            ensureDir dir
            d <- genInitData
            writeJSON iFile d
            runData $ do
                gitInit
                makeGitCommit "Initial commit"
