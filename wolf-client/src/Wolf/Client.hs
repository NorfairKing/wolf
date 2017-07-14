{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Client where

import Servant.API
import Servant.Client

import Wolf.API
import Wolf.Types

clientPostNewPerson :: PersonEntry -> ClientM PersonUuid
clientGetPersonEntry :: PersonUuid -> ClientM PersonEntry
clientPostNewPerson :<|> clientGetPersonEntry = client wolfAPI
