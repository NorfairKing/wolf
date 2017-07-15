module Wolf.Cli.OptParse.Types where

import Import

import Wolf.Data.Types

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandInit
    | CommandNote String
    | CommandSummary String
    | CommandEntry String
    | CommandGit [String]
    | CommandAlias String
                   String
    | CommandReview
    deriving (Show, Eq)

newtype Flags = Flags
    { flagDataFlags :: DataFlags
    } deriving (Show, Eq)

newtype DataFlags = DataFlags
    { dataFlagWolfDir :: Maybe FilePath
    } deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch
    = DispatchInit
    | DispatchNote String
    | DispatchSummary String
    | DispatchEntry String
    | DispatchGit [String]
    | DispatchAlias String
                    String
    | DispatchReview
    deriving (Show, Eq)

newtype Settings = Settings
    { setDataSets :: DataSettings
    } deriving (Show, Eq)

data ParserEnv = ParserEnv
    { parserEnvDefaultWolfDir :: Path Abs Dir
    , parserEnvIndex :: Index
    } deriving (Show, Eq)
