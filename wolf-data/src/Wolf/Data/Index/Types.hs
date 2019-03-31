{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Index.Types
  ( Alias(..)
  , alias
  , aliasString
  , Index(..)
  , newIndex
  , lookupInIndex
  , addIndexEntry
  ) where

import Import

import Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

import Wolf.Data.People.Types

newtype Alias =
  Alias
    { aliasText :: Text
    }
  deriving ( Show
           , Eq
           , Hashable
           , Ord
           , Generic
           , FromJSONKey
           , ToJSONKey
           , FromJSON
           , ToJSON
           )

alias :: Text -> Alias
alias = Alias

aliasString :: Alias -> String
aliasString = T.unpack . aliasText

instance IsString Alias where
  fromString = Alias . T.pack

instance Validity Alias

instance NFData Alias

newtype Index =
  Index
    { indexMap :: Map Alias PersonUuid
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity Index

instance NFData Index

instance FromJSON Index where
  parseJSON v =
    withObject "Index" (\o -> Index <$> o .: "indexMap") v <|>
    (Index <$> parseJSON v)

instance ToJSON Index where
  toJSON (Index m) = toJSON m

newIndex :: Index
newIndex = Index {indexMap = M.empty}

-- | Look up a `PersonUuid` in the 'Index' by its alias
lookupInIndex :: Alias -> Index -> Maybe PersonUuid
lookupInIndex person index = M.lookup person (indexMap index)

-- | Add a 'PersonUuid' to the 'Index' at an alias
addIndexEntry :: Alias -> PersonUuid -> Index -> Index
addIndexEntry person uuid origIndex =
  origIndex {indexMap = M.insert person uuid $ indexMap origIndex}
