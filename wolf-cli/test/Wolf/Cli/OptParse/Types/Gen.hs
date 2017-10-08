{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Cli.OptParse.Types.Gen where

import TestImport

import Wolf.Cli.OptParse.Types

import Wolf.Data.Gen ()

instance GenUnchecked Command

instance GenValid Command

instance GenUnchecked SuggestionFlags

instance GenValid SuggestionFlags

instance GenUnchecked Flags

instance GenValid Flags

instance GenUnchecked DataFlags

instance GenValid DataFlags

instance GenUnchecked Configuration

instance GenValid Configuration

instance GenUnchecked Dispatch

instance GenValid Dispatch

instance GenUnchecked PeriodDescription

instance GenValid PeriodDescription

instance GenUnchecked SuggestionSettings

instance GenValid SuggestionSettings

instance GenUnchecked Settings

instance GenValid Settings

instance GenUnchecked ParserEnv

instance GenValid ParserEnv
