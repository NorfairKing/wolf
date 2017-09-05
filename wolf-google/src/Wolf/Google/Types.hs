{-# LANGUAGE DeriveGeneric #-}

module Wolf.Google.Types where

import Import

import Data.Aeson as JSON

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
