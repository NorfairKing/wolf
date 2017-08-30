{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Server.AccountServerSpec
    ( spec
    ) where

import TestImport

import Network.HTTP.Types
import Servant.Client

import Wolf.API
import Wolf.Client

import Wolf.API.Gen ()
import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.Types.Gen ()

import Wolf.Server.TestUtils

spec :: Spec
spec =
    withWolfServer $
    describe "postRegister" $ do
        it "does not crash" $ \cenv ->
            forAll genValid $ \register -> do
                errOrUuid <- runClient cenv $ clientPostRegister register
                case errOrUuid of
                    Left err ->
                        let snf =
                                expectationFailure $
                                "Should not fail with error: " <> show err
                        in case err of
                               FailureResponse {} ->
                                   if statusCode (responseStatus err) == 409
                                       then pure ()
                                       else snf
                               _ -> snf
                    Right uuid -> uuid `shouldSatisfy` isValid
        it "returns a 409 error if the username already exists" $ \cenv ->
            forAll genValid $ \register ->
                forAll genValid $ \secondPassword -> do
                    void $ runClientOrError cenv $ clientPostRegister register
                    errOrUuid <-
                        runClient cenv $
                        clientPostRegister $
                        register {registerPassword = secondPassword}
                    case errOrUuid of
                        Left err ->
                            let snf =
                                    expectationFailure $
                                    "Should not fail with error: " <> show err
                            in case err of
                                   FailureResponse {} ->
                                       if statusCode (responseStatus err) == 409
                                           then pure ()
                                           else snf
                                   _ -> snf
                        Right _ ->
                            expectationFailure "Should not have succeeded."
