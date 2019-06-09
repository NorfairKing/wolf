{-# LANGUAGE DeriveGeneric #-}

module Wolf.Server.Types
  ( WolfHandler
  , WolfServerEnv(..)
  ) where

import Import

import Servant

type WolfHandler = ReaderT WolfServerEnv Handler

data WolfServerEnv =
  WolfServerEnv
    { wseDataDir :: Path Abs Dir
    , wseGitExecutable :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)
