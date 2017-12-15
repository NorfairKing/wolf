{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Web.Server.Application where

import Yesod
import Yesod.Auth

import Wolf.Web.Server.Foundation
import Wolf.Web.Server.Handler.Home
import Wolf.Web.Server.Handler.NewPerson
import Wolf.Web.Server.Handler.NewNote
import Wolf.Web.Server.Handler.Person

mkYesodDispatch "App" resourcesApp
