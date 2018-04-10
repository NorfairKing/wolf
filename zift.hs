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

import Data.List (sort)

main :: IO ()
main =
    ziftWith $ do
        preprocessor hindentZift
        prechecker gitAddAllZift
        checker $ do
            hlintZift
            pkgs_ <- stackGetPackages
            let pkgs =
                    [ Pkg "wolf-data" [Lib "wolf-data:lib"]
                    , Pkg
                          "wolf-test-utils"
                          [ Lib "wolf-test-utils:lib"
                          , Test "wolf-test-utils:test:wolf-data-test"
                          , Bench "wolf-test-utils:bench:wolf-data-bench"
                          ]
                    , Pkg
                          "wolf-data-baked"
                          [ Lib "wolf-data-baked:lib"
                          , Test "wolf-data-baked:test:wolf-data-baked-test"
                          ]
                    , Pkg
                          "wolf-api"
                          [ Lib "wolf-api:lib"
                          , Test "wolf-api:test:wolf-api-test"
                          ]
                    , Pkg
                          "wolf-cli"
                          [ Lib "wolf-cli:lib"
                          , Test "wolf-cli:test:wolf-cli-test"
                          ]
                    , Pkg "wolf-client" [Lib "wolf-client:lib"]
                    , Pkg
                          "wolf-server"
                          [ Lib "wolf-server:lib"
                          , Test "wolf-server:test:wolf-server-test"
                          ]
                    , Pkg "wolf-cub" [Lib "wolf-cub:lib"]
                    , Pkg
                          "wolf-google"
                          [ Lib "wolf-google:lib"
                          , Test "wolf-google:test:wolf-google-test"
                          ]
                    , Pkg
                          "wolf-web-server"
                          [ Lib "wolf-web-server:lib"
                          , Test "wolf-web-server:test:wolf-web-server-test"
                          ]
                    ]
            if sort pkgs_ == sort pkgs
                then do
                    stack "build"
                    mapM_ bePedanticAboutPackage pkgs
                else fail "Not all packages are pedanticly built."
