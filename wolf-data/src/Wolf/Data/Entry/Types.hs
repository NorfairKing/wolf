{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Entry.Types where

import Import

import System.IO.Unsafe -- TODO remove this

import Data.Aeson as JSON
import qualified Data.Map as M
import Data.Time

{-# ANN module ("HLint: ignore Use &&" :: Text) #-}

newtype PersonEntry = PersonEntry
    { personEntryProperties :: [(Text, PersonPropertyValue)]
    } deriving (Show, Eq, Ord, Generic)

instance Validity PersonEntry where
    isValid PersonEntry {..} =
        and
            [ isValid personEntryProperties
            , let ls = map fst personEntryProperties
              in nub ls == ls
            ]

instance FromJSON PersonEntry where
    parseJSON ob =
        (withObject "PersonEntry" $ \o -> do
             strs <- o .: "personEntryProperties"
             pure
                 PersonEntry
                 { personEntryProperties =
                       map
                           (second
                                (`PersonPropertyValue` unsafePerformIO
                                                           getCurrentTime))
                           (M.toList strs)
                 })
            ob <|>
        (withObject "PersonEntry" $ \o -> PersonEntry <$> o .: "properties") ob

instance ToJSON PersonEntry where
    toJSON PersonEntry {..} = object ["properties" .= personEntryProperties]

newPersonEntry :: PersonEntry
newPersonEntry = PersonEntry {personEntryProperties = []}

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
