{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Server.IntegrationSpec
    ( spec
    ) where

import TestImport

import Wolf.Client

import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.Types.Gen ()

import Wolf.Server.TestUtils

spec :: Spec
spec =
    withWolfServer $
    describe "integration tests" $
    describe "getPersonEntry" $
    it "gets the same person that was just posted" $ \cenv ->
        forAll genValid $ \pe -> do
            pe' <-
                runClientOrError cenv $ do
                    uuid <- clientPostNewPerson pe
                    clientGetPersonEntry uuid
            pe' `shouldBe` pe
