{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Wolf.Web.Server.Handler.Home where

import Import

import Yesod
import Yesod.Auth

import Wolf.Web.Server.Foundation

getHomeR :: Handler Html
getHomeR = do
    mauth <- maybeAuthId
    case mauth of
        Nothing -> redirect $ AuthR LoginR
        Just _ -> do
            token <- genToken
            withNavBar $ do
                setTitle "Wolf"
                $(widgetFile "home/logged-in")
