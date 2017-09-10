{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Index.Types
    ( Index(..)
    , newIndex
    ) where

import Import

import Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M

import Wolf.Data.People.Types

newtype Index = Index
    { indexMap :: Map Text PersonUuid
    } deriving (Show, Eq, Ord, Generic)

instance Validity Index

instance FromJSON Index

instance ToJSON Index

newIndex :: Index
newIndex = Index {indexMap = M.empty}
