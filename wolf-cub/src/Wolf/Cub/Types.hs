{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cub.Types where

import Import

import Data.String

import Brick.Widgets.List (List)

import Wolf.Data.Entry.Types
import Wolf.Data.Types

data CubState = CubState
    { cubStatePersonList :: List ResourceName (Text, PersonUuid)
    , cubStatePopupUuid :: Maybe (PersonUuid, Maybe PersonEntry)
    , cubStateDataSettings :: DataSettings
    } deriving (Show, Generic)

newtype ResourceName =
    ResourceName String
    deriving (Show, Read, Eq, Ord, Generic)

instance IsString ResourceName where
    fromString = ResourceName
