module Wolf.OptParse.Types where

import Import

import Wolf.Types

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandNote String
    | CommandSummary String
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch
    = DispatchNote String
    | DispatchSummary String
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

newtype ParserEnv = ParserEnv
    { parserEnvIndex :: Index
    } deriving (Show, Eq)
