{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types
    ( Export(..)
    ) where

import Import

import Data.Aeson

import Wolf.Data.Index.Types
import Wolf.Data.Init.Types

data Export = Export
    { exportInitData :: InitData
    , exportPersonIndex :: Maybe Index
    } deriving (Show, Eq, Generic)

instance Validity Export

instance NFData Export

instance FromJSON Export where
    parseJSON =
        withObject "Export" $ \o ->
            Export <$> o .: "init-data" <*> o .: "person-index"

instance ToJSON Export where
    toJSON Export {..} =
        object
            ["init-data" .= exportInitData, "person-index" .= exportPersonIndex]
