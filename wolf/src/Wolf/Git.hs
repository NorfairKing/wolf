module Wolf.Git where

import Import

import System.Process

import Wolf.Path

git :: [String] -> IO ()
git = liftIO . runGit

gitInit :: MonadIO m => m ()
gitInit = runGit ["init"]

makeGitCommit :: MonadIO m => String -> m ()
makeGitCommit message = do
    runGit ["add", "."]
    runGit ["commit", "--message", show message]

runGit :: MonadIO m => [String] -> m ()
runGit args = do
    let gitcmd = "git"
    let cmd = unwords $ gitcmd : args
    wd <- wolfDir
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
