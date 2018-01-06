{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Baked where

import Import

import Wolf.Data

class FromProperty a where
    fromProperty :: PersonProperty -> a

atKey :: FromProperty (Maybe a) => PersonProperty -> Text -> Maybe a
atKey (PMap tups) key = lookup key tups >>= fromProperty
atKey _ _ = Nothing

fromEntry :: FromProperty a => PersonEntry -> a
fromEntry = fromProperty . personEntryProperties

instance FromProperty (Maybe Text) where
    fromProperty (PVal ppv) = Just $ personPropertyValueContents ppv
    fromProperty _ = Nothing

newtype Met = Met
    { metText :: Text
    } deriving (Show, Eq, Generic)

instance Validity Met

instance FromProperty (Maybe Met) where
    fromProperty pp = Met <$> pp `atKey` "met"

data Gender
    = Male
    | Female
    | Other Text
    deriving (Show, Eq, Generic)

instance Validity Gender

instance FromProperty (Maybe Gender) where
    fromProperty pp =
        flip fmap (pp `atKey` "gender") $ \gt ->
            case gt of
                "male" -> Male
                "female" -> Female
                _ -> Other gt

data Name = Name
    { namePrefix :: Maybe Text -- Mr
    , nameFirst :: Maybe Text -- John
    , nameMiddle :: Maybe Text -- Jonas
    , nameLast :: Maybe Text -- Smith
    , nameSuffix :: Maybe Text -- Jr
    , nameNick :: Maybe Text -- Joe
    } deriving (Show, Eq, Generic)

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
