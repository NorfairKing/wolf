{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wolf.Web.Server.Handler.NewPerson where

import Import

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time

import Yesod

import Wolf.Web.Server.Foundation

import Wolf.Data

getNewPersonR :: Handler Html
getNewPersonR = do
    ix <- runData getIndexWithDefault
    let il = sortOn snd $ M.toList $ reverseIndexSingleAlias ix
    token <- genToken
    defaultLayout $ withNavBar $(widgetFile "new-person")

newtype NewPerson = NewPerson
    { newPersonAlias :: Alias
    }

newPersonForm :: FormInput Handler NewPerson
newPersonForm = NewPerson <$> ireq aliasField "alias"

postNewPersonR :: Handler Html
postNewPersonR = do
    NewPerson {..} <- runInputPost newPersonForm
    now <- liftIO getCurrentTime
    puuid <-
        runData $ do
            ix <- getIndexWithDefault
            (puuid, ix') <- lookupOrCreateNewPerson newPersonAlias ix
            putIndex ix'
            pure puuid
    redirect $ PersonR puuid
