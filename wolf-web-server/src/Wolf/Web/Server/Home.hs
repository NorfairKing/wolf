{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Wolf.Web.Server.Home where

import Import

import Yesod

import Wolf.Web.Server.Foundation

getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        setTitle "Wolf"
        [whamlet|
        <h1> Wolf
    |]
