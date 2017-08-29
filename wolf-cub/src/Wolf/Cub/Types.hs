{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cub.Types where

import Import

import Data.String

import Brick.Widgets.List (List)

import Wolf.Data.Entry.Types
import Wolf.Data.Types

data CubState = CubState
    { cubStateShown :: CubShown
    , cubStateDataSettings :: DataSettings
    } deriving (Show, Generic)

data CubShown = CubShown
    { cubShownPersonList :: List ResourceName (Text, PersonUuid)
    , cubShownPopupUuid :: Maybe (PersonUuid, Maybe PersonEntry)
    } deriving (Show, Generic)

newtype ResourceName =
    ResourceName String
    deriving (Show, Read, Eq, Ord, Generic)

instance IsString ResourceName where
    fromString = ResourceName
