{-# LANGUAGE DeriveGeneric #-}

module Wolf.Cub.Types where

import Import

data CubState =
    CubState
    deriving (Show, Read, Eq, Generic)
