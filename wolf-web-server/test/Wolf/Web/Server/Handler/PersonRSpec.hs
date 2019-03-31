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
  yit "returns a 200 for each person" $
  withExampleAccount $ \uuid -> do
    puuid <-
      runTestDataShared uuid $ do
        ix <- getIndexWithDefault
        let al = alias "alias"
        (puuid, ix') <- lookupOrCreateNewPerson al ix
        putIndex ix'
        pure puuid
    get $ PersonR puuid
    statusIs 200
