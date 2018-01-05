#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter
    --package zifter-cabal
    --package zifter-git
    --package zifter-hindent
    --package zifter-hlint
    --package zifter-stack
-}
import Zifter
import Zifter.Cabal
import Zifter.Git
import Zifter.Hindent
import Zifter.Hlint
import Zifter.Stack

import Control.Monad
import Control.Monad.IO.Class
import Path
import Path.IO
import System.Process

main :: IO ()
main =
    ziftWith $ do
        recursiveZift
        preprocessor hindentZift
        prechecker gitAddAllZift
        checker $ do
            rd <- getRootDir
            liftIO $ do
                sd <- resolveDir rd "wolf-web-server/static"
                (_, _, _, ph) <-
                    createProcess
                        (shell "./set-up-semantic.sh")
                        {cwd = Just $ toFilePath sd}
                void $ waitForProcess ph
            hlintZift
            stackBuildZift
