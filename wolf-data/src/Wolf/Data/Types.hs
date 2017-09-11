{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Types where

import Import

import Data.Aeson as JSON
import Data.Time

{-# ANN module ("HLint: ignore Use &&" :: Text) #-}

data InitData = InitData
    { initDataDir :: Path Abs Dir
    , initTimestamp :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity InitData

instance FromJSON InitData

instance ToJSON InitData

newtype DataSettings = DataSettings
    { dataSetWolfDir :: Path Abs Dir
    } deriving (Show, Eq)
