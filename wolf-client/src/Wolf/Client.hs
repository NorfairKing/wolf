{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Client
    ( PersonUuid
    , PersonEntry
    , Text
    , clientGetPersonEntry
    , clientPostNewPerson
    , clientGetPersonByAlias
    , SetPersonAlias(..)
    , clientPostSetPersonAlias
    , PersonQuery
    , clientGetPersonQuery
    ) where

import Data.Text

import Servant.API
import Servant.Client

import Wolf.API
import Wolf.Data.Entry.Types
import Wolf.Data.Types

clientGetPersonEntry :: PersonUuid -> ClientM PersonEntry
clientPostNewPerson :: PersonEntry -> ClientM PersonUuid
clientGetPersonByAlias :: Text -> ClientM PersonUuid
clientPostSetPersonAlias :: SetPersonAlias -> ClientM ()
clientGetPersonQuery :: PersonQuery -> ClientM [PersonUuid]
clientGetPersonEntry :<|> clientPostNewPerson :<|> clientGetPersonByAlias :<|> clientPostSetPersonAlias :<|> clientGetPersonQuery =
    client wolfAPI
