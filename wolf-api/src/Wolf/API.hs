{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.API where

import Data.Proxy

import Servant.API

import Wolf.Types

type WolfAPI = PersonAPI

wolfAPI :: Proxy WolfAPI
wolfAPI = Proxy

type PersonAPI = PostNewPerson :<|> GetPersonEntry

type PostNewPerson
     = "person" :> "new" :> ReqBody '[ JSON] PersonEntry :> Post '[ JSON] PersonUuid

type GetPersonEntry
     = "person" :> Capture "person-uuid" PersonUuid :> Get '[ JSON] PersonEntry
