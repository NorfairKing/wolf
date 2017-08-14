{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Client where

import Data.Text

import Servant.API
import Servant.Client

import Wolf.API
import Wolf.Data.Types

clientGetPersonEntry :: PersonUuid -> ClientM PersonEntry
clientPostNewPerson :: PersonEntry -> ClientM PersonUuid
clientGetPerson :: Text -> ClientM PersonUuid
clientGetPersonQuery :: PersonQuery -> ClientM [PersonUuid]
clientGetPersonEntry :<|> clientPostNewPerson :<|> clientGetPerson :<|> clientGetPersonQuery =
    client wolfAPI
