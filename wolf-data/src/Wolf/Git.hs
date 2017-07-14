{-# LANGUAGE FlexibleContexts #-}

module Wolf.Git where

import Import

import System.Process

import Wolf.Types
import Wolf.Path

gitInit :: (MonadReader DataSettings m, MonadIO m) => m ()
gitInit = runGit ["init"]

makeGitCommit :: (MonadReader DataSettings m, MonadIO m) => String -> m ()
makeGitCommit message = do
    runGit ["add", "."]
    runGit ["commit", "--message", show message]

runGit :: (MonadReader DataSettings m, MonadIO m) => [String] -> m ()
runGit args = do
    wd <- wolfDir
    runGitIn wd args

runGitIn :: MonadIO m => Path Abs Dir -> [String] -> m ()
runGitIn wd args = do
    let gitcmd = "git"
    let cmd = unwords $ gitcmd : args
    let cp =
            (proc gitcmd args)
            { cwd = Just $ toFilePath wd
            , std_in = Inherit
            , std_out = Inherit
            , std_err = Inherit
            }
    ec <-
        liftIO $ do
            (_, _, _, ph) <- createProcess cp
            waitForProcess ph
    case ec of
        ExitFailure code ->
            liftIO $ die $ unwords [cmd, "failed with exit code", show code]
        ExitSuccess -> pure ()
