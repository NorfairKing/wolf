{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Server.AccountsSpec
    ( spec
    ) where

import TestImport

import qualified Data.Map as M

import Wolf.API
import Wolf.Server.Accounts

import Wolf.API.Gen ()
import Wolf.Data.Gen ()

import Wolf.Server.TestUtils

spec :: Spec
spec = do
    genValidSpec @Account
    withTestSandbox $ do
        describe "getAccounts" $ do
            it "gets an empty map in an empty state" $ \env ->
                withEnv env getAccounts `shouldReturn` M.empty
            it "gets the accounts that were just stored" $ \env ->
                forAllValid $ \m -> do
                    m' <-
                        withEnv env $ do
                            storeAccounts m
                            getAccounts
                    m' `shouldBe` m
            it "gets the two accounts that were just stored" $ \env ->
                forAll (genValid `suchThat` uncurry ((/=) `on` accountUsername)) $ \(acc1, acc2) -> do
                    muuids <-
                        withEnv env $ do
                            m1 <- tryToAddNewAccount (accountUsername acc1)
                            m2 <- tryToAddNewAccount (accountUsername acc2)
                            pure $ (,) <$> m1 <*> m2
                    case muuids of
                        Nothing ->
                            expectationFailure
                                "Should have been able to add the accounts."
                        Just (u1, u2) -> do
                            as <-
                                withEnv env $ do
                                    storeAccount $ acc1 {accountUUID = u1}
                                    storeAccount $ acc2 {accountUUID = u2}
                                    getAccounts
                            as `shouldBe`
                                M.fromList
                                    [ (accountUsername acc1, u1)
                                    , (accountUsername acc2, u2)
                                    ]
        describe "tryToAddNewAccount" $
            it "successfully adds a new account to an empty state" $ \env ->
                forAllValid $ \un -> do
                    m <- withEnv env $ tryToAddNewAccount un
                    m `shouldSatisfy` isJust
        describe "lookupAccountUUID" $
            it "succesfully looks up newly registered accounts" $ \env ->
                forAllValid $ \un -> do
                    mm <-
                        withEnv env $ do
                            m1 <- tryToAddNewAccount un
                            m2 <- lookupAccountUUID un
                            pure (m1, m2)
                    case mm of
                        (Nothing, _) ->
                            expectationFailure
                                "Should not have failed to add the account."
                        (Just _, Nothing) ->
                            expectationFailure
                                "Should not have failed to look up the account."
                        (Just u1, Just u2) -> u2 `shouldBe` u1
        -- WTF is happening here
        -- describe "getAccount" $
        --     it "gets the account that was just stored" $ \env ->
        --         forAllValid $ \acc -> do
        --             ma <-
        --                 withEnv env $ do
        --                     storeAccount acc
        --                     getAccount $ accountUUID $ traceShowId acc
        --             ma `shouldBe` Just acc
