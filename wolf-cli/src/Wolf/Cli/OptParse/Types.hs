{-# LANGUAGE DeriveGeneric #-}

module Wolf.Cli.OptParse.Types where

import Import

import Wolf.Data

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
  = CommandInit
  | CommandNote (Maybe Text) [Text]
  | CommandSummary Text
  | CommandEntry Text
  | CommandGit [String]
  | CommandAlias Text Text
  | CommandReview (Maybe PeriodDescription)
  | CommandRandomPerson
  | CommandSuggestion SuggestionFlags
  | CommandExport
  | CommandCleanup
  deriving (Show, Eq, Generic)

instance Validity Command

data SuggestionFlags
  = CommandListSuggestions
  | CommandReviewSuggestion
  deriving (Show, Eq, Generic)

instance Validity SuggestionFlags

newtype Flags =
  Flags
    { flagDataFlags :: DataFlags
    }
  deriving (Show, Eq, Generic)

instance Validity Flags

data DataFlags =
  DataFlags
    { dataFlagWolfDir :: Maybe FilePath
    , dataFlagGitExecutable :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

instance Validity DataFlags

data Configuration =
  Configuration
  deriving (Show, Eq, Generic)

instance Validity Configuration

data Dispatch
  = DispatchInit
  | DispatchNote [Alias]
  | DispatchSummary Alias
  | DispatchEntry Alias
  | DispatchGit [String]
  | DispatchAlias Alias Alias
  | DispatchReview PeriodDescription
  | DispatchRandomPerson
  | DispatchSuggestion SuggestionSettings
  | DispatchExport
  | DispatchCleanup
  deriving (Show, Eq, Generic)

instance Validity Dispatch

data PeriodDescription
  = LastDay
  | LastWeek
  | LastMonth
  deriving (Show, Eq, Generic)

instance Validity PeriodDescription

data SuggestionSettings
  = DispatchListSuggestions
  | DispatchReviewSuggestion
  deriving (Show, Eq, Generic)

instance Validity SuggestionSettings

newtype Settings =
  Settings
    { setDataSets :: DataSettings
    }
  deriving (Show, Eq, Generic)

instance Validity Settings

data ParserEnv =
  ParserEnv
    { parserEnvDefaultWolfDir :: Path Abs Dir
    , parserEnvIndex :: Index
    }
  deriving (Show, Eq, Generic)

instance Validity ParserEnv
