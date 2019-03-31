{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Server.PersonServerSpec
  ( spec
  ) where

import TestImport

import Wolf.Client

import Wolf.API.Gen ()
import Wolf.Data.Gen ()
import Wolf.Server.TestUtils

spec :: Spec
spec =
  withWolfServer $ do
    describe "getPersonEntry" $
      it "gets the same person that was just posted" $ \cenv ->
        once $
        forAllValid $ \pe ->
          withValidNewUser cenv $ \ad -> do
            pe' <-
              runClientOrError cenv $ do
                uuid <- clientPostNewPerson ad pe
                clientGetPersonEntry ad uuid
            pe' `shouldBe` pe
    describe "getPersonByAlias" $
      it "gets the person that just had its alias set" $ \cenv ->
        once $
        forAllValid $ \pe ->
          forAllValid $ \alias ->
            withValidNewUser cenv $ \ad -> do
              (uuid, uuid') <-
                runClientOrError cenv $ do
                  uuid <- clientPostNewPerson ad pe
                  clientPostSetPersonAlias
                    ad
                    SetPersonAlias
                      { setPersonAliasPersonUuid = uuid
                      , setPersonAliasAlias = alias
                      }
                  uuid' <- clientGetPersonByAlias ad alias
                  pure (uuid, uuid')
              uuid' `shouldBe` uuid
