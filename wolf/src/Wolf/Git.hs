module Wolf.Git where

import Import

import System.Process

import Wolf.Path

git :: [String] -> IO ()
git args = do
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
            die $ unwords [cmd, "failed with exit code", show code]
        ExitSuccess -> pure ()
