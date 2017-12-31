module Wolf.Web.Server.OptParse.Types where

import Import

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandServe ServeFlags
    deriving (Show, Eq)

data ServeFlags = ServeFlags
    { serveFlagPort :: Maybe Int
    , serveFlagDataDir :: Maybe FilePath
    , serveFlagAPIPort :: Maybe Int
    } deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Environment = Environment
    { envPort :: Maybe Int
    , envDataDir :: Maybe FilePath
    , envAPIPort :: Maybe Int
    } deriving (Show, Eq)

newtype Dispatch =
    DispatchServe ServeSettings
    deriving (Show, Eq)

data ServeSettings = ServeSettings
    { serveSetPort :: Int
    , serveSetDataDir :: Path Abs Dir
    , serveSetAPIPort :: Maybe Int
    } deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)
