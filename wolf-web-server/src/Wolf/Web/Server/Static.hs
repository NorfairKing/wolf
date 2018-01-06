{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Web.Server.Static where

import Import

import System.Exit
import System.Process

import Language.Haskell.TH

import Yesod.EmbeddedStatic

do let callIn cmd dir = do
           let cp = (shell cmd) {cwd = Just dir}
           (_, _, _, ph) <- createProcess cp
           ec <- waitForProcess ph
           case ec of
               ExitSuccess -> pure ()
               ExitFailure c ->
                   die $
                   unwords ["failed to install semantic-ui with code", show c]
   runIO $ do
       callIn "npm install semantic-ui --save" "static"
       callIn "gulp build" "static/semantic"
   mkEmbeddedStatic
       False
       "myStatic"
       [ embedFile "static/semantic/dist/semantic.min.css"
       , embedFile "static/semantic/dist/semantic.min.js"
       , embedDirAt
             "static/semantic/dist/themes/default/assets/fonts"
             "static/semantic/dist/themes/default/assets/fonts"
       ]
