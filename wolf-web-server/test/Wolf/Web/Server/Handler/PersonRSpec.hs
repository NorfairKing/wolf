{-# LANGUAGE OverloadedStrings #-}

module Wolf.Web.Server.Handler.PersonRSpec where

import TestImport

import Yesod.Test

import Wolf.Data

import Wolf.Web.Server.Foundation
import Wolf.Web.Server.TestUtils

spec :: Spec
spec =
    wolfWebServerPersonalSpec $
    ydescribe "PersonRSpec" $
    yit "returns a 200 for each person" $ do
        uuid <-
            runTestDataPersonal $ do
                ix <- getIndexWithDefault
                let al = alias "alias"
                (uuid, ix') <- lookupOrCreateNewPerson al ix
                putIndex ix'
                pure uuid
        get $ PersonR uuid
        statusIs 200
