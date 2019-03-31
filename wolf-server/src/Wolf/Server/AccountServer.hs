{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.AccountServer
  ( accountServer
  ) where

import Import

import Control.Monad.Except

import Servant.Server

import Wolf.API

import Wolf.Server.Accounts
import Wolf.Server.Types

accountServer :: ServerT AccountAPI WolfHandler
accountServer = servePostRegister

servePostRegister :: Register -> WolfHandler AccountUUID
servePostRegister reg = do
  errOrId <- registerAccount reg
  case errOrId of
    Left InvalidPassword ->
      throwError $ err400 {errBody = "Failed to hash password."}
    Left UsernameExists ->
      throwError $
      err409 {errBody = "Account with this username already exists."}
    Right uuid -> pure uuid
