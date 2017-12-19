{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Wolf.Web.Server.Handler.Home where

import Import

import qualified Data.Map as M

import Yesod
import Yesod.Auth

import Wolf.API
import Wolf.Data

import Wolf.Web.Server.Foundation

getHomeR :: Handler Html
getHomeR = do
    ix <- runData getIndexWithDefault
    multiUser <- isMultiUser
    let il =
            sortOn snd $ M.toList $ reverseIndexSingleAlias ix :: [( PersonUuid
                                                                   , Alias)]
    mauth <- maybeAuthId
    ar <-
        do ar <- getApprootText approot <$> getYesod <*> waiRequest
           pure $
               case ar of
                   "" -> "wolf.example.com"
                   _ -> ar
    defaultLayout $ do
        setTitle "Wolf"
        $(widgetFile "home")
