{-# LANGUAGE TemplateHaskell #-}

module Wolf.Web.Server.Handler.PersonEntry where

import Import

import Yesod

import Wolf.Data

import Wolf.Web.Server.Foundation

getPersonEntryR :: PersonUuid -> Handler Html
getPersonEntryR uuid = do
    (mpe, ix) <-
        runData $ do
            mpe <- getPersonEntry uuid
            ix <- getIndexWithDefault
            pure (mpe, ix)
    let malias = reverseIndexLookupSingleAlias uuid ix
    defaultLayout $(widgetFile "person-entry")
