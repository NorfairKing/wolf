{-# LANGUAGE DeriveGeneric #-}

module Wolf.Server.Types where

import Import

import Control.Monad.Reader

import Servant

import Wolf.Data.Types

type WolfHandler = ReaderT WolfServerEnv Handler

newtype WolfServerEnv = WolfServerEnv
    { wseDataSettings :: DataSettings
    } deriving (Show, Eq, Generic)
