{-# LANGUAGE OverloadedStrings #-}

module Wolf.Web.Server.Handler.NewNoteRSpec where

import TestImport

import Yesod.Test

import Wolf.Data

import Wolf.Web.Server.Application ()
import Wolf.Web.Server.Foundation
import Wolf.Web.Server.TestUtils

spec :: Spec
spec =
    wolfWebServerSpec $
    ydescribe "NewNoteR" $ do
        yit "returns a 200" $ do
            get NewNoteR
            statusIs 200
        yit "it allows submitting of the form" $ do
            uuids <- runTestData getPersonUuids
            case uuids of
                [] -> lift $ expectationFailure "No person to add a note to."
                (uuid:_) -> do
                    get NewNoteR
                    statusIs 200
                    request $ do
                        setMethod "POST"
                        setUrl NewNoteR
                        addTokenFromCookie
                        addPostParam "contents" "test contents"
                        addPostParam "uuid" $ personUuidText uuid
                    statusIs 303
                    loc <- getLocation
                    lift $ loc `shouldBe` Right HomeR
