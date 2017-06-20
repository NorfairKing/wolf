module Wolf.OptParse.Types where

import Import

import Wolf.Types

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
    { flagWolfDir :: Maybe FilePath
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
    { setWolfDir :: Path Abs Dir
    } deriving (Show, Eq)

data ParserEnv = ParserEnv
    { parserEnvDefaultWolfDir :: Path Abs Dir
    , parserEnvIndex :: Index
    } deriving (Show, Eq)
