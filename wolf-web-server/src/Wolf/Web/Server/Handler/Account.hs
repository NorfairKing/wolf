{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Wolf.Web.Server.Handler.Account where

import Import

import Yesod
import Yesod.Auth

import Wolf.Web.Server.Foundation

getAccountR :: Handler Html
getAccountR = do
    void requireAuthId
    token <- genToken
    -- FIXME: make the example independent of where this is run.
    withNavBar $ do
        setTitle "Wolf Account"
        $(widgetFile "account")
