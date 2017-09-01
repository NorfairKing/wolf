module Wolf.Google.OptParse.Types where

import Import

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command =
    CommandSuggest
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch =
    DispatchSuggest
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)
