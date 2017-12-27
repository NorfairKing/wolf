{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wolf.Web.Server.Handler.NewPerson where

import Import

import Yesod

import Wolf.Web.Server.Foundation

import Wolf.Data
import Wolf.Data.Git

getNewPersonR :: Handler Html
getNewPersonR = do
    token <- genToken
    withNavBar $(widgetFile "new-person")

newtype NewPerson = NewPerson
    { newPersonAlias :: Alias
    }

newPersonForm :: FormInput Handler NewPerson
newPersonForm = NewPerson <$> ireq aliasField "alias"

postNewPersonR :: Handler Html
postNewPersonR = do
    NewPerson {..} <- runInputPost newPersonForm
    puuid <-
        runData $ do
            ix <- getIndexWithDefault
            (puuid, ix') <- lookupOrCreateNewPerson newPersonAlias ix
            putIndex ix'
            makeGitCommit $
                unwords
                    ["Added new person with alias", aliasString newPersonAlias]
            pure puuid
    redirect $ PersonR puuid
