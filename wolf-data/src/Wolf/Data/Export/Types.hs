{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types
    ( Export(..)
    ) where

import Import

import Data.Aeson

import Wolf.Data.Init.Types

data Export = Export
    { exportInitData :: InitData
    } deriving (Show, Eq, Generic)

instance Validity Export

instance NFData Export

instance FromJSON Export where
    parseJSON = withObject "Export" $ \o -> Export <$> o .: "init-data"

instance ToJSON Export where
    toJSON Export {..} = object ["init-data" .= exportInitData]
