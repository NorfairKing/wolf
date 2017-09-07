{-# LANGUAGE DeriveGeneric #-}

module Wolf.Google.Suggest.Types where

import Import

import Data.Aeson as JSON

import Wolf.Data

data GatheredPerson = GatheredPerson
    { gatheredPersonAliases :: [Text]
    , gatheredPersonNames :: [GatheredName]
    , gatheredPersonEmails :: [Text]
    , gatheredPersonPhoneNumbers :: [Text]
    } deriving (Show, Eq, Generic)

instance FromJSON GatheredPerson

instance ToJSON GatheredPerson

data GatheredName = GatheredName
    { gatheredNamePrefix :: Maybe Text
    , gatheredNameFirstName :: Maybe Text
    , gatheredNameMiddleName :: Maybe Text
    , gatheredNameLastName :: Maybe Text
    , gatheredNameSuffix :: Maybe Text
    } deriving (Show, Eq, Generic)

instance FromJSON GatheredName

instance ToJSON GatheredName

data PersonContext = PersonContext
    { personContextUuid :: PersonUuid
    , personContextAliases :: [Text]
    , personContextEntry :: Maybe PersonEntry
    } deriving (Show, Eq, Generic)

instance FromJSON PersonContext

instance ToJSON PersonContext
