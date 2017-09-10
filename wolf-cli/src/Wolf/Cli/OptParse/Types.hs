module Wolf.Cli.OptParse.Types where

import Import

import Wolf.Data

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandInit
    | CommandNote (Maybe Text)
                  [Text]
    | CommandSummary Text
    | CommandEntry Text
    | CommandGit [String]
    | CommandAlias Text
                   Text
    | CommandReview
    | CommandRandomPerson
    | CommandSuggestion SuggestionFlags
    deriving (Show, Eq)

data SuggestionFlags
    = CommandListSuggestions
    | CommandReviewSuggestion
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
    | DispatchNote [Text]
    | DispatchSummary Text
    | DispatchEntry Text
    | DispatchGit [String]
    | DispatchAlias Text
                    Text
    | DispatchReview
    | DispatchRandomPerson
    | DispatchSuggestion SuggestionSettings
    deriving (Show, Eq)

data SuggestionSettings
    = DispatchListSuggestions
    | DispatchReviewSuggestion
    deriving (Show, Eq)

newtype Settings = Settings
    { setDataSets :: DataSettings
    } deriving (Show, Eq)

data ParserEnv = ParserEnv
    { parserEnvDefaultWolfDir :: Path Abs Dir
    , parserEnvIndex :: Index
    } deriving (Show, Eq)
