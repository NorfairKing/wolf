{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Types where

import Import

import Data.Aeson

newtype DataSettings = DataSettings
    { dataSetWolfDir :: Path Abs Dir
    } deriving (Show, Eq, Generic)

instance Validity DataSettings

instance NFData DataSettings

instance FromJSON DataSettings where
    parseJSON =
        withObject "DataSettings" $ \o -> DataSettings <$> o .: "wolf-data-dir"

instance ToJSON DataSettings where
    toJSON DataSettings {..} = object ["wolf-data-dir" .= dataSetWolfDir]
