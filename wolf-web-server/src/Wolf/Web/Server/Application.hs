{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Web.Server.Application where

import Yesod

import Wolf.Web.Server.Foundation
import Wolf.Web.Server.Handler.Home

mkYesodDispatch "App" resourcesApp
