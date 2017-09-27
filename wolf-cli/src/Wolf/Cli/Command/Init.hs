{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Init where

import Import

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data
import Wolf.Data.Git

init :: (MonadIO m, MonadReader Settings m) => m ()
init = do
    mid <- runData readInitData
    case mid of
        Just d ->
            liftIO $
            die $
            unwords
                [ "A wolf repository has already been initialised in"
                , toFilePath $ initDataDir d
                , "on"
                , show $ initTimestamp d
                ]
        Nothing ->
            runData $ do
                initWolf
                gitInit
                makeGitCommit "Initial commit"
