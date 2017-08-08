module Wolf.Cub.OptParse.Types where

import Import

import Wolf.Data.Types

import qualified Wolf.Cli.OptParse.Types as Cli

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandRun RunFlags
    deriving (Show, Eq)

newtype RunFlags = RunFlags
    { flagDataFlags :: Cli.DataFlags
    } deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

newtype Dispatch =
    DispatchRun RunSettings
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

newtype RunSettings = RunSettings
    { runSetDataSettings :: DataSettings
    } deriving (Show, Eq)
