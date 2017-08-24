{-# LANGUAGE DeriveGeneric #-}

module Wolf.Server.Types
    ( WolfHandler
    , WolfServerEnv(..)
    ) where

import Import

import Servant

type WolfHandler = ReaderT WolfServerEnv Handler

newtype WolfServerEnv = WolfServerEnv
    { wseDataDir :: Path Abs Dir
    } deriving (Show, Eq, Generic)
