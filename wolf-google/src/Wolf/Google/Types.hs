{-# LANGUAGE DeriveGeneric #-}

module Wolf.Google.Types where

import Import

import Data.Aeson as JSON

import Wolf.Data

data GatheredPerson = GatheredPerson
    { gatheredPersonAliases :: [Text]
    , gatheredPersonEntry :: PersonProperty
    } deriving (Show, Eq, Generic)

instance FromJSON GatheredPerson

instance ToJSON GatheredPerson
