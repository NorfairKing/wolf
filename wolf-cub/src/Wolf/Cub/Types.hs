{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cub.Types where

import Import

import Data.String

import Brick.Widgets.List (List)

import Wolf.Data

data CubState = CubState
    { cubStateShown :: CubShown
    , cubStateDataSettings :: DataSettings
    } deriving (Show, Generic)

data CubShown
    = CubShowPersonList (List ResourceName (Text, PersonUuid))
    | CubShowPerson PersonState
    deriving (Show, Generic)

data PersonState = PersonState
    { personStateUuid :: PersonUuid
    , personStateEntry :: Maybe PersonEntry
    } deriving (Show, Generic)

newtype ResourceName =
    ResourceName String
    deriving (Show, Read, Eq, Ord, Generic)

instance IsString ResourceName where
    fromString = ResourceName
