{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Index.Types
    ( Alias(..)
    , alias
    , aliasString
    , Index(..)
    , newIndex
    ) where

import Import

import Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

import Wolf.Data.People.Types

newtype Alias = Alias
    { aliasText :: Text
    } deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey)

alias :: Text -> Alias
alias = Alias

aliasString :: Alias -> String
aliasString = T.unpack . aliasText

instance IsString Alias where
    fromString = Alias . T.pack

instance Validity Alias

instance NFData Alias

instance FromJSON Alias

instance ToJSON Alias

newtype Index = Index
    { indexMap :: Map Alias PersonUuid
    } deriving (Show, Eq, Ord, Generic)

instance Validity Index

instance NFData Index

instance FromJSON Index

instance ToJSON Index

newIndex :: Index
newIndex = Index {indexMap = M.empty}
