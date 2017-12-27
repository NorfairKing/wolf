{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Wolf.Web.Server.Handler.Home where

import Import

import qualified Data.Map as M

import Yesod
import Yesod.Auth

import Wolf.Data

import Wolf.Web.Server.Foundation

getHomeR :: Handler Html
getHomeR = do
    ix <- runData getIndexWithDefault
    let il =
            sortOn snd $ M.toList $ reverseIndexSingleAlias ix :: [( PersonUuid
                                                                   , Alias)]
    mauth <- maybeAuthId
    -- FIXME: make the example independent of where this is run.
    withNavBar $ do
        setTitle "Wolf"
        $(widgetFile "home")
