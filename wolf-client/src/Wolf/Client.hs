{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.Client
    ( Register(..)
    , AccountUUID
    , clientPostRegister
    , PersonUuid
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
import Wolf.Data

clientPostRegister :: Register -> ClientM AccountUUID
clientPostRegister = accountClient

clientGetPersonEntry :: BasicAuthData -> PersonUuid -> ClientM PersonEntry
clientPostNewPerson :: BasicAuthData -> PersonEntry -> ClientM PersonUuid
clientGetPersonByAlias :: BasicAuthData -> Alias -> ClientM PersonUuid
clientPostSetPersonAlias :: BasicAuthData -> SetPersonAlias -> ClientM ()
clientGetPersonQuery :: BasicAuthData -> PersonQuery -> ClientM [PersonUuid]
clientGetPersonEntry :<|> clientPostNewPerson :<|> clientGetPersonByAlias :<|> clientPostSetPersonAlias :<|> clientGetPersonQuery =
    personClient

accountClient :: Client ClientM AccountAPI
personClient :: Client ClientM PersonAPI
accountClient :<|> personClient = wolfClient

wolfClient :: Client ClientM WolfAPI
wolfClient = client wolfAPI
