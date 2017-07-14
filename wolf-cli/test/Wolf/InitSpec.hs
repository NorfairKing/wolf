module Wolf.InitSpec
    ( spec
    ) where

import TestImport

-- import TestUtils
-- import Wolf.Command.Init
-- import Wolf.Cli.OptParse.Types
spec :: Spec
spec = pure ()
    -- describe "init" $ do
    --     withSandbox $
    --         it "does not fail in a nonexistent sandbox directory" $ \sb ->
    --             runReaderT init Settings {setWolfDir = sb} :: IO ()
    --     withSandbox $
    --         it "does not fail in an existent sandbox directory" $ \sb -> do
    --             ensureDir sb
    --             runReaderT init Settings {setWolfDir = sb} :: IO ()
    --     withSandbox $
    --         it "fails when a wolf dir has already been initialised" $ \sb -> do
    --             runReaderT init Settings {setWolfDir = sb}
    --             runReaderT init Settings {setWolfDir = sb} `shouldThrow`
    --                 (== ExitFailure 1)
