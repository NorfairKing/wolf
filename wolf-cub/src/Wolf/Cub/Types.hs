{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cub.Types where

import Import

import Data.String
import Data.Time

import Brick.Widgets.List (List)

import Wolf.Data

data CubState = CubState
    { cubStateShown :: CubShown
    , cubStateNow :: UTCTime
    , cubStateDataSettings :: DataSettings
    } deriving (Show, Generic)

data CubShown
    = CubShowPersonList (List ResourceName (Text, PersonUuid))
    | CubShowPerson PersonState
    deriving (Show, Generic)

data PersonState = PersonState
    { personStateUuid :: PersonUuid
    , personStateEntry :: Maybe PersonEntry
    , personStateNotes :: List ResourceName (NoteUuid, Note)
    } deriving (Show, Generic)

newtype ResourceName =
    ResourceName String
    deriving (Show, Read, Eq, Ord, Generic)

instance IsString ResourceName where
    fromString = ResourceName
