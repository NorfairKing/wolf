{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Types where

import Import

import Data.Aeson

data DataSettings =
  DataSettings
    { dataSetWolfDir :: Path Abs Dir
    , dataSetGitExecutable :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

instance Validity DataSettings

instance NFData DataSettings

instance FromJSON DataSettings where
  parseJSON =
    withObject "DataSettings" $ \o ->
      DataSettings <$> o .: "wolf-data-dir" <*> o .: "git-executable"

instance ToJSON DataSettings where
  toJSON DataSettings {..} =
    object ["wolf-data-dir" .= dataSetWolfDir, "git-executable" .= dataSetGitExecutable]
