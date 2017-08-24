{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Server.AccountServer
    ( accountServer
    ) where

import Import

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE

import Control.Monad.Except

import Servant.API
import Servant.Server

import Wolf.Data.Entry.Types
import Wolf.Data.Index
import Wolf.Data.Types

import Wolf.API

import Wolf.Server.Types
import Wolf.Server.Utils

accountServer :: ServerT AccountAPI WolfHandler
accountServer = servePostRegister

servePostRegister :: Register -> WolfHandler ()
servePostRegister _ = pure ()
