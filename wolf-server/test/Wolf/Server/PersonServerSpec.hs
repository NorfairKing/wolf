{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Server.PersonServerSpec
    ( spec
    ) where

import TestImport

import Wolf.Client

import Wolf.Data.Entry.Types.Gen ()
import Wolf.Data.Types.Gen ()

import Wolf.Server.TestUtils

spec :: Spec
spec =
    withWolfServer $ do
        describe "getPersonEntry" $
            it "gets the same person that was just posted" $ \cenv ->
                forAll genValid $ \pe -> do
                    pe' <-
                        runClientOrError cenv $ do
                            uuid <- clientPostNewPerson pe
                            clientGetPersonEntry uuid
                    pe' `shouldBe` pe
        describe "getPersonByAlias" $
            it "gets the person that just had its alias set" $ \cenv ->
                forAll genValid $ \pe ->
                    forAll genValid $ \alias -> do
                        (uuid, uuid') <-
                            runClientOrError cenv $ do
                                uuid <- clientPostNewPerson pe
                                clientPostSetPersonAlias
                                    SetPersonAlias
                                    { setPersonAliasPersonUuid = uuid
                                    , setPersonAliasAlias = alias
                                    }
                                uuid' <- clientGetPersonByAlias alias
                                pure (uuid, uuid')
                        uuid' `shouldBe` uuid
