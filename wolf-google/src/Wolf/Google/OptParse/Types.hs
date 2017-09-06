module Wolf.Google.OptParse.Types where

import Import

import qualified Wolf.Cli.OptParse.Types as Cli
import Wolf.Data.Types

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command =
    CommandSuggest Cli.DataFlags
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch =
    DispatchSuggest DataSettings
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)
