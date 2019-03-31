{-# LANGUAGE OverloadedStrings #-}

module Wolf.Web.Server.Handler.AuthRSpec where

import TestImport

import Network.HTTP.Types

import Yesod.Test

import Wolf.Web.Server.Foundation
import Wolf.Web.Server.TestUtils

spec :: Spec
spec =
  wolfWebServerPersonalSpec $
  ydescribe "RegisterR" $ do
    yit "gets a 200" $ do
      get $ AuthR registerR
      statusIs 200
    yit "registers an example account correctly" $ do
      let exampleUsername = "example"
      let examplePassphrase = "example"
      get $ AuthR registerR
      statusIs 200
      request $ do
        setMethod methodPost
        setUrl $ AuthR registerR
        addTokenFromCookie
        addPostParam "username" exampleUsername
        addPostParam "passphrase" examplePassphrase
        addPostParam "passphrase-confirm" examplePassphrase
      statusIs 303
      loc <- getLocation
      liftIO $ loc `shouldBe` Right HomeR
