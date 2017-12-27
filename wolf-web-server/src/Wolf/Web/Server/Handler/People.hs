{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Wolf.Web.Server.Handler.People where

import Import

import qualified Data.Map as M

import Yesod
import Yesod.Auth

import Wolf.Data

import Wolf.Web.Server.Foundation

getPeopleR :: Handler Html
getPeopleR = do
    ix <- runData getIndexWithDefault
    let il =
            sortOn snd $ M.toList $ reverseIndexSingleAlias ix :: [( PersonUuid
                                                                   , Alias)]
    mauth <- maybeAuthId
    withNavBar $ do
        setTitle "Wolf People"
        $(widgetFile "people")
