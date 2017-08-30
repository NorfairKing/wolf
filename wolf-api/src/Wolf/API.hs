{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.API
    ( wolfAPI
    , WolfAPI
    , AccountAPI
    , PostRegister
    , Register(..)
    , AccountUUID
    , newAccountUUID
    , accountUUIDString
    , accountUUIDText
    , Username
    , validUsernameChar
    , username
    , usernameText
    , PasswordHash
    , hashPassword
    , validatePassword
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

import Data.Aeson as JSON
import Data.Aeson.Types as JSON (toJSONKeyText)
import qualified Data.ByteString.Base16 as Base16
import Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID as UUID
import Data.UUID.V4 as UUID

import qualified Crypto.BCrypt as BCrypt

import Servant.API

import Wolf.Data

wolfAPI :: Proxy WolfAPI
wolfAPI = Proxy

type WolfAPI = AccountAPI :<|> PersonAPI

type AccountAPI = PostRegister

newtype AccountUUID = AccountUUID
    { unAccountUUID :: UUID
    } deriving (Show, Eq, Generic)

instance Validity AccountUUID where
    isValid = const True

instance FromJSON AccountUUID where
    parseJSON =
        withText "AccountUUID" $ \t ->
            case UUID.fromText t of
                Nothing -> fail "Invalid Text when parsing UUID"
                Just u -> pure $ AccountUUID u

instance ToJSON AccountUUID where
    toJSON (AccountUUID u) = JSON.String $ UUID.toText u

newAccountUUID :: IO AccountUUID
newAccountUUID = AccountUUID <$> UUID.nextRandom

accountUUIDString :: AccountUUID -> String
accountUUIDString = UUID.toString . unAccountUUID

accountUUIDText :: AccountUUID -> Text
accountUUIDText = UUID.toText . unAccountUUID

newtype Username = Username
    { usernameText :: Text
    } deriving (Show, Eq, Ord, Generic)

instance Validity Username where
    isValid (Username t) = T.all validUsernameChar t

validUsernameChar :: Char -> Bool
validUsernameChar c =
    not (Char.isControl c) && Char.isAlphaNum c && Char.isLatin1 c

instance FromJSONKey Username where
    fromJSONKey = FromJSONKeyTextParser parseUsername

instance ToJSONKey Username where
    toJSONKey = toJSONKeyText (\(Username t) -> t)

instance FromJSON Username where
    parseJSON = withText "Username" parseUsername

parseUsername :: MonadFail m => Text -> m Username
parseUsername t =
    case constructValid t of
        Nothing -> fail "Invalid username in JSON"
        Just un -> pure $ Username un

instance ToJSON Username where
    toJSON (Username un) = toJSON un

newtype PasswordHash =
    PasswordHash ByteString
    deriving (Show, Eq, Generic)

username :: Text -> Maybe Username
username t = Username <$> constructValid t

instance Validity PasswordHash

instance FromJSON PasswordHash where
    parseJSON =
        withText "PasswordHash" $ \t ->
            case Base16.decode $ TE.encodeUtf8 t of
                (h, "") -> pure $ PasswordHash h
                _ ->
                    fail
                        "Invalid password hash in JSON: could not decode from hex string"

instance ToJSON PasswordHash where
    toJSON (PasswordHash bs) =
        case TE.decodeUtf8' $ Base16.encode bs of
            Left _ ->
                error "Failed to decode hex string to text, should not happen."
            Right t -> JSON.String t

hashPassword :: Text -> IO (Maybe PasswordHash)
hashPassword =
    fmap (fmap PasswordHash) .
    BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy .
    TE.encodeUtf8

validatePassword :: PasswordHash -> ByteString -> Bool
validatePassword (PasswordHash h) = BCrypt.validatePassword h

data Account = Account
    { accountUUID :: AccountUUID
    , accountUsername :: Username
    , accountPasswordHash :: PasswordHash
    } deriving (Show, Eq, Generic)

instance Validity Account

instance FromJSON Account

instance ToJSON Account

type PostRegister
     = "account" :> "register" :> ReqBody '[ JSON] Register :> Post '[ JSON] AccountUUID

data Register = Register
    { registerUsername :: Username
    , registerPassword :: Text
    } deriving (Show, Eq, Generic)

instance Validity Register

instance FromJSON Register

instance ToJSON Register

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
