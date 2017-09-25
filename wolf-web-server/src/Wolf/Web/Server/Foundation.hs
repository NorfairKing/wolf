{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Wolf.Web.Server.Foundation where

import Yesod

data App =
    App

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App
