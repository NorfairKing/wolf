{-# LANGUAGE OverloadedStrings #-}

module Wolf.Web.Server.Handler.NewNoteRSpec where

import TestImport

import Yesod.Test

import Network.HTTP.Types

import Wolf.Data
import Wolf.Data.Git

import Wolf.Web.Server.Application ()
import Wolf.Web.Server.Foundation
import Wolf.Web.Server.TestUtils

spec :: Spec
spec =
    wolfWebServerPersonalSpec $
    ydescribe "NewNoteR" $ do
        yit "returns a 200" $
            withExampleAccount_ $ do
                get NewNoteR
                statusIs 200
        yit "it allows submitting of the form" $
            withExampleAccount $ \uuid -> do
                let newPersonAlias = "test-alias"
                uuid <-
                    runTestDataShared uuid $ do
                        ix <- getIndexWithDefault
                        (puuid, ix') <-
                            lookupOrCreateNewPerson newPersonAlias ix
                        putIndex ix'
                        makeGitCommit $
                            unwords
                                [ "Added new person with alias"
                                , aliasString newPersonAlias
                                ]
                        pure puuid
                get NewNoteR
                statusIs 200
                request $ do
                    setMethod methodPost
                    setUrl NewNoteR
                    addTokenFromCookie
                    addPostParam "contents" "test contents"
                    addPostParam "uuid" $ personUuidText uuid
                statusIs 303
                loc <- getLocation
                lift $ loc `shouldBe` Right HomeR
