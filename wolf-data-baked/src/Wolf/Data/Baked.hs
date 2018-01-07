{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Baked
    ( module Wolf.Data.Baked
    , module Wolf.Data.Baked.Name
    , module Wolf.Data.Baked.FromProperty
    , module Wolf.Data.Baked.Suggestion.Alias
    ) where

import Import

import Wolf.Data.Baked.FromProperty
import Wolf.Data.Baked.Name
import Wolf.Data.Baked.Suggestion.Alias

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
