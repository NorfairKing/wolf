{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.API
    ( wolfAPI
    , WolfAPI
    , AccountAPI
    , PersonAPI
    , PostRegister
    , Register(..)
    , AccountUUID
    , newAccountUUID
    , accountUUIDString
    , accountUUIDText
    , Account(..)
    , PersonAPI
    , GetPersonEntry
    , PostNewPerson
    , GetPersonByAlias
    , PostPersonSetAlias
    , SetPersonAlias(..)
    , GetPersonQuery
    , PersonQuery(..)
    ) where

import Import

import Data.Aeson
import Data.UUID as UUID
import Data.UUID.V4 as UUID


import Servant.API

import Wolf.Data.Entry.Types
import Wolf.Data.Types

wolfAPI :: Proxy WolfAPI
wolfAPI = Proxy

type WolfAPI = AccountAPI :<|> PersonAPI

type AccountAPI = PostRegister

type PostRegister
     = "account" :> "register" :> ReqBody '[ JSON] Register :> Post '[ JSON] ()

data Register = Register
    { registerUsername :: Text
    , registerPassword :: Text
    } deriving (Show, Eq, Generic)

instance Validity Register

instance FromJSON Register

instance ToJSON Register

newtype AccountUUID = AccountUUID
    { unAccountUUID :: UUID
    } deriving (Show, Eq, Generic)

newAccountUUID :: IO AccountUUID
newAccountUUID = AccountUUID <$> UUID.nextRandom

accountUUIDString :: AccountUUID -> String
accountUUIDString = UUID.toString . unAccountUUID

accountUUIDText :: AccountUUID -> Text
accountUUIDText = UUID.toText . unAccountUUID

data Account = Account
    { accountUUID :: AccountUUID
    , accountUsername :: Text
    , accountPasswordHash :: ByteString
    } deriving (Show, Eq, Generic)

type Protected = BasicAuth "master" Account

type PersonAPI
     = GetPersonEntry :<|> PostNewPerson :<|> GetPersonByAlias :<|> PostPersonSetAlias :<|> GetPersonQuery

type GetPersonEntry
     = Protected :> "person" :> "entry" :> Capture "person-uuid" PersonUuid :> Get '[ JSON] PersonEntry

type PostNewPerson
     = Protected :> "person" :> "new" :> ReqBody '[ JSON] PersonEntry :> Post '[ JSON] PersonUuid

type GetPersonByAlias
     = Protected :> "person" :> "by-alias" :> ReqBody '[ JSON] Text :> Get '[ JSON] PersonUuid

type PostPersonSetAlias
     = Protected :> "person" :> "alias" :> ReqBody '[ JSON] SetPersonAlias :> Post '[ JSON] ()

data SetPersonAlias = SetPersonAlias
    { setPersonAliasPersonUuid :: PersonUuid
    , setPersonAliasAlias :: Text
    } deriving (Show, Eq, Generic)

instance Validity SetPersonAlias

instance FromJSON SetPersonAlias

instance ToJSON SetPersonAlias

type GetPersonQuery
     = Protected :> "person" :> "by-entry-query" :> ReqBody '[ JSON] PersonQuery :> Get '[ JSON] [PersonUuid]

data PersonQuery
    = EntryValue Text
                 Text
    | AndQuery PersonQuery
               PersonQuery
    deriving (Show, Read, Eq, Generic)

instance Validity PersonQuery

instance FromJSON PersonQuery

instance ToJSON PersonQuery
