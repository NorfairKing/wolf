{-# LANGUAGE DeriveGeneric #-}

module Wolf.Email.OptParse.Types where

import Import

import Wolf.Data

import Wolf.Cli.OptParse.Types (DataFlags(..))

import Wolf.Email.Suggest.Types

data Arguments =
    Arguments Command
              Flags
    deriving (Show, Eq, Generic)

data Instructions =
    Instructions Dispatch
                 Settings
    deriving (Show, Eq, Generic)

data Command =
    CommandSuggest
    deriving (Show, Eq, Generic)

instance Validity Command

newtype Flags = Flags
    { flagDataFlags :: DataFlags
    } deriving (Show, Eq, Generic)

instance Validity Flags

data Configuration =
    Configuration
    deriving (Show, Eq, Generic)

instance Validity Configuration

data Dispatch =
    DispatchSuggest
    deriving (Show, Eq, Generic)

instance Validity Dispatch

newtype Settings = Settings
    { setDataSets :: DataSettings
    } deriving (Show, Eq, Generic)

instance Validity Settings
