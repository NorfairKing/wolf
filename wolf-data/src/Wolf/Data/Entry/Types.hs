{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Entry.Types
    ( PersonEntry
    , personEntry
    , personEntryProperty
    , newPersonEntry
    , PersonProperty(..)
    , WithLastChanged(..)
    ) where

import Import

import System.IO.Unsafe -- TODO remove this

import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Scientific
import Data.Time
import qualified Data.Vector as V

{-# ANN module ("HLint: ignore Use &&" :: Text) #-}

newtype PersonEntry = PersonEntry
    { personEntryProperties :: PersonProperty
    } deriving (Show, Eq, Ord, Generic)

-- | Make a person entry, return 'Nothing' if it wouldn't be valid.
personEntry :: PersonProperty -> Maybe PersonEntry
personEntry = constructValid . PersonEntry

-- | Get the individual entry list out of a person entry.
personEntryProperty :: PersonEntry -> PersonProperty
personEntryProperty = personEntryProperties

instance Validity PersonEntry

instance FromJSON PersonEntry where
    parseJSON ob =
        (withObject "PersonEntry" $ \o -> do
             let now = unsafePerformIO getCurrentTime
                 wls = WithLastChanged now
             strs <- o .: "personEntryProperties"
             pure
                 PersonEntry
                 { personEntryProperties =
                       PMapping $
                       flip map (M.toList strs) $ \(k, t) ->
                           (k, PString (wls t))
                 })
            ob <|>
        (withObject "PersonEntry" $ \o -> PersonEntry <$> o .: "properties") ob

instance ToJSON PersonEntry where
    toJSON PersonEntry {..} = object ["properties" .= personEntryProperties]

newPersonEntry :: PersonEntry
newPersonEntry = PersonEntry {personEntryProperties = PMapping []}

data PersonProperty
    = PString (WithLastChanged Text)
    | PBool (WithLastChanged Bool)
    | PNumber (WithLastChanged Scientific)
    | PList [PersonProperty]
    | PMapping [(Text, PersonProperty)] -- ^ Order, but no duplicate keys and not empty
    deriving (Show, Eq, Ord, Generic)

-- | A 'PersonProperty' if its components are valid.
--
-- For 'PMapping', that means that there must not be duplicate keys, but they are ordered.
--
-- TODO validity of the timestamps recursively
instance Validity PersonProperty where
    isValid (PString t) = isValid t
    isValid (PBool b) = isValid b
    isValid (PNumber n) = isValid n
    isValid (PList l) = isValid l
    isValid (PMapping m) =
        and
            [ isValid m
            , not (null m)
            , let ls = map fst m
              in nub ls == ls
            ]

instance FromJSON PersonProperty where
    parseJSON (JSON.Array v) = parseList <|> parseMapping
      where
        parseMapping =
            fmap (PMapping . V.toList) $
            forM v $
            withObject "KeyValue Pair" $ \o -> do
                k <- o .: "key"
                v <- o .: "val"
                pure (k, v)
        parseList = (PList . V.toList) <$> mapM parseJSON v
    parseJSON o = do
        wlc <- parseJSON o
        let lcf = WithLastChanged (lastChanged wlc)
        case lastChangedValue wlc of
            JSON.String t -> pure $ PString $ lcf t
            JSON.Bool b -> pure $ PBool $ lcf b
            JSON.Number n -> pure $ PNumber $ lcf n

instance ToJSON PersonProperty where
    toJSON (PString wls) = toJSON wls
    toJSON (PBool wls) = toJSON wls
    toJSON (PNumber wls) = toJSON wls
    toJSON (PList ls) = toJSON ls
    toJSON (PMapping m) =
        flip listValue m $ \(k, v) -> object ["key" .= k, "val" .= v]

data WithLastChanged a = WithLastChanged
    { lastChanged :: UTCTime
    , lastChangedValue :: a
    } deriving (Show, Eq, Ord, Functor, Generic)

instance Validity a => Validity (WithLastChanged a)

instance FromJSON a => FromJSON (WithLastChanged a) where
    parseJSON =
        withObject "WithLastChanged" $ \o ->
            WithLastChanged <$> o .: "value" <*>
            (o .: "last-updated" <|> o .: "last-changed")

instance ToJSON a => ToJSON (WithLastChanged a) where
    toJSON WithLastChanged {..} =
        object ["value" .= lastChanged, "last-changed" .= lastChangedValue]
