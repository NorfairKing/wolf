{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Wolf.Web.Server.Handler.Home where

import Import

import qualified Data.Map as M

import Yesod
import Yesod.Auth

import Wolf.Data
import Wolf.API

import Wolf.Web.Server.Foundation

getHomeR :: Handler Html
getHomeR = do
    ix <- runData getIndexWithDefault
    multiUser <- isMultiUser
    let il =
            sortOn snd $ M.toList $ reverseIndexSingleAlias ix :: [( PersonUuid
                                                                   , Alias)]
    mauth <- maybeAuthId
    defaultLayout $ do
        setTitle "Wolf"
        $(widgetFile "home")
