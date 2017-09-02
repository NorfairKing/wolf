{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Entry.Types
    ( PersonEntry
    , personEntry
    , personEntryProperties
    , newPersonEntry
    , PersonProperty(..)
    , PersonPropertyValue(..)
    ) where

import Import

import System.IO.Unsafe -- TODO remove this

import Data.Aeson as JSON
import qualified Data.Map as M
import Data.Time

{-# ANN module ("HLint: ignore Use &&" :: Text) #-}

newtype PersonEntry = PersonEntry
    { personEntryProperties :: PersonProperty -- ^ Get the individual entry list out of a person entry.
    } deriving (Show, Eq, Ord, Generic)

-- | Make a person entry, return 'Nothing' if it wouldn't be valid.
personEntry :: PersonProperty -> Maybe PersonEntry
personEntry = constructValid . PersonEntry

-- | A 'PersonEntry' is valid if it does not have duplicate keys.
instance Validity PersonEntry

instance FromJSON PersonEntry where
    parseJSON ob =
        (withObject "PersonEntry" $ \o -> do
             strs <- o .: "personEntryProperties"
             pure
                 PersonEntry
                 { personEntryProperties =
                       PMap $
                       map
                           (second
                                (PVal .
                                 (`PersonPropertyValue` unsafePerformIO
                                                            getCurrentTime)))
                           (M.toList strs)
                 })
            ob <|>
        (withObject "PersonEntry" $ \o -> PersonEntry <$> o .: "properties") ob

instance ToJSON PersonEntry where
    toJSON PersonEntry {..} = object ["properties" .= personEntryProperties]

newPersonEntry :: PersonEntry
newPersonEntry = PersonEntry {personEntryProperties = PMap []}

data PersonProperty
    = PVal PersonPropertyValue
    | PList [PersonProperty]
    | PMap [(Text, PersonProperty)]
    deriving (Show, Eq, Ord, Generic)

instance Validity PersonProperty where
    isValid (PVal ppv) = isValid ppv
    isValid (PList pl) = isValid pl
    isValid (PMap tups) =
        and
            [ isValid tups
            , not (null tups)
            , let ls = map fst tups
              in nub ls == ls
            ]

instance ToJSON PersonProperty where
    toJSON (PVal pv) = toJSON pv
    toJSON (PList pl) = toJSON pl
    toJSON (PMap tups) = toJSON tups

instance FromJSON PersonProperty where
    parseJSON o =
        (PVal <$> parseJSON o) <|> (PList <$> parseJSON o) <|>
        (PMap <$> parseJSON o)

data PersonPropertyValue = PersonPropertyValue
    { personPropertyValueContents :: Text
    , personPropertyValueLastUpdatedTimestamp :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity PersonPropertyValue

instance FromJSON PersonPropertyValue where
    parseJSON =
        withObject "PersonPropertyValue" $ \o ->
            PersonPropertyValue <$> o .: "value" <*> o .: "last-updated"

instance ToJSON PersonPropertyValue where
    toJSON PersonPropertyValue {..} =
        object
            [ "value" .= personPropertyValueContents
            , "last-updated" .= personPropertyValueLastUpdatedTimestamp
            ]
