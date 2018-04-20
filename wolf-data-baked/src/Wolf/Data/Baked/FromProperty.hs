{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Wolf.Data.Baked.FromProperty where

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

instance FromProperty (Maybe PersonProperty) where
    fromProperty = pure
