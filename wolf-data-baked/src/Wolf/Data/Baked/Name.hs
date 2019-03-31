{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Baked.Name where

import Import

import qualified Data.Text as T

import Wolf.Data.Baked.FromProperty

data Name =
  Name
    { namePrefix :: Maybe Text -- Mr
    , nameFirst :: Maybe Text -- John
    , nameMiddle :: Maybe Text -- Jonas
    , nameLast :: Maybe Text -- Smith
    , nameSuffix :: Maybe Text -- Jr
    , nameNick :: Maybe Text -- Joe
    }
  deriving (Show, Eq, Generic)

instance Validity Name

instance FromProperty Name where
  fromProperty pp =
    Name
      { namePrefix = pp `atKey` "prefix"
      , nameFirst = pp `atKey` "first name"
      , nameMiddle = pp `atKey` "middle name"
      , nameLast = pp `atKey` "last name"
      , nameSuffix = pp `atKey` "suffix"
      , nameNick = pp `atKey` "nick name"
      }

renderName :: Name -> Maybe Text
renderName Name {..} =
  case (nameFirst, nameLast) of
    (Nothing, Nothing) -> Nothing
    (Just fn, Nothing) -> Just $ T.unwords $ fn : maybeToList nameMiddle
    (Nothing, Just ln) -> Just $ "Mr or Ms " <> ln
    (Just fn, Just ln) ->
      Just $ T.unwords $ [fn] ++ maybeToList nameMiddle ++ [ln]
