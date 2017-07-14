module Wolf.Server.OptParse.Types where

import Import

import Wolf.Types

import qualified Wolf.Cli.OptParse.Types as Cli

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandServe ServeFlags
    deriving (Show, Eq)

newtype ServeFlags = ServeFlags
    { serveFlagPort :: Maybe Int
    } deriving (Show, Eq)

newtype Flags = Flags
    { flagDataFlags :: Cli.DataFlags
    } deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

newtype Dispatch =
    DispatchServe ServeSettings
    deriving (Show, Eq)

newtype Settings = Settings
    { setDataSettings :: DataSettings
    } deriving (Show, Eq)

newtype ServeSettings = ServeSettings
    { serveSetPort :: Int
    } deriving (Show, Eq)
