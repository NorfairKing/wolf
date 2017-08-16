{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.API where

import Import

import Data.Aeson

import Servant.API

import Wolf.Data.Entry.Types
import Wolf.Data.Types

type WolfAPI = PersonAPI

wolfAPI :: Proxy WolfAPI
wolfAPI = Proxy

type PersonAPI
     = GetPersonEntry :<|> PostNewPerson :<|> GetPerson :<|> GetPersonQuery

type GetPersonEntry
     = "person" :> "entry" :> Capture "person-uuid" PersonUuid :> Get '[ JSON] PersonEntry

type PostNewPerson
     = "person" :> "new" :> ReqBody '[ JSON] PersonEntry :> Post '[ JSON] PersonUuid

type GetPerson
     = "person" :> "by-key" :> Capture "person-key" Text :> Get '[ JSON] PersonUuid

type GetPersonQuery
     = "person" :> "by-entry-query" :> ReqBody '[ JSON] PersonQuery :> Get '[ JSON] [PersonUuid]

data PersonQuery
    = EntryValue Text
                 Text
    | AndQuery PersonQuery
               PersonQuery
    deriving (Show, Read, Eq, Generic)

instance Validity PersonQuery

instance FromJSON PersonQuery

instance ToJSON PersonQuery
