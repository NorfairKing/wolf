module Wolf.Web.Server.OptParse.Types where

import Import

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandServe ServeFlags
    deriving (Show, Eq)

data ServeFlags = ServeFlags
    { serveFlagPort :: Maybe Int
    , serveFlagDataFlags :: Maybe DataFlags
    } deriving (Show, Eq)

data DataFlags
    = PersonalFlags FilePath
    | SharedFlags FilePath
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Environment = Environment
    { envPort :: Maybe Int
    , envDataFlags :: Maybe DataFlags
    } deriving (Show, Eq)

newtype Dispatch =
    DispatchServe ServeSettings
    deriving (Show, Eq)

data ServeSettings = ServeSettings
    { serveSetPort :: Int
    , serveSetDataSets :: DataSettings
    } deriving (Show, Eq)

data DataSettings
    = PersonalSets (Path Abs Dir)
    | SharedSets (Path Abs Dir)
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)
