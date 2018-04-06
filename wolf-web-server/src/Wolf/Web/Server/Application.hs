{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Web.Server.Application where

import Yesod
import Yesod.Auth

import Wolf.Web.Server.Foundation
import Wolf.Web.Server.Handler.Account
import Wolf.Web.Server.Handler.Graph
import Wolf.Web.Server.Handler.Home
import Wolf.Web.Server.Handler.NewNote
import Wolf.Web.Server.Handler.NewPerson
import Wolf.Web.Server.Handler.People
import Wolf.Web.Server.Handler.Person
import Wolf.Web.Server.Handler.Suggestions
import Wolf.Web.Server.Handler.Suggestions.Alias

mkYesodDispatch "App" resourcesApp
