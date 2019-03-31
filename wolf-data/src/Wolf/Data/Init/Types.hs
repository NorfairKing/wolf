{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Init.Types
  ( InitData(..)
  ) where

import Import

import Data.Aeson as JSON
import Data.Time

data InitData =
  InitData
    { initDataDir :: Path Abs Dir
    , initTimestamp :: UTCTime
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity InitData

instance NFData InitData

instance FromJSON InitData where
  parseJSON =
    withObject "InitData" $ \o ->
      InitData <$> (o .: "initDataDir" <|> o .: "data-dir") <*>
      (o .: "initTimestamp" <|> o .: "timestamp")

instance ToJSON InitData where
  toJSON InitData {..} =
    object ["data-dir" .= initDataDir, "timestamp" .= initTimestamp]
