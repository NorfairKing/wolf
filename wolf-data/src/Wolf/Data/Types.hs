{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Types where

import Import

import System.IO.Unsafe -- TODO remove this

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Servant

{-# ANN module ("HLint: ignore Use &&" :: Text) #-}

newtype InitData = InitData
    { initTimestamp :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity InitData

instance FromJSON InitData

instance ToJSON InitData

newtype Index = Index
    { indexMap :: Map Text PersonUuid
    } deriving (Show, Eq, Ord, Generic)

instance Validity Index

instance FromJSON Index

instance ToJSON Index

newIndex :: Index
newIndex = Index {indexMap = M.empty}

newtype PersonUuid = PersonUuid
    { unPersonUuid :: UUID
    } deriving (Show, Eq, Ord, Generic)

personUuidBs :: PersonUuid -> ByteString
personUuidBs (PersonUuid uuid) = UUID.toASCIIBytes uuid

personUuidLBs :: PersonUuid -> LB.ByteString
personUuidLBs = LB.fromStrict . personUuidBs

personUuidString :: PersonUuid -> String
personUuidString (PersonUuid uuid) = UUID.toString uuid

instance Validity PersonUuid where
    isValid = const True

instance FromJSON PersonUuid where
    parseJSON =
        withText "PersonUuid" $ \t ->
            case UUID.fromText t of
                Nothing -> fail "Invalid Text when parsing UUID"
                Just u -> pure $ PersonUuid u

instance ToJSON PersonUuid where
    toJSON (PersonUuid u) = JSON.String $ UUID.toText u

instance FromHttpApiData PersonUuid where
    parseUrlPiece t =
        case UUID.fromText t of
            Nothing -> fail $ "Invalid UUID in Url Piece: " ++ T.unpack t
            Just uuid -> pure $ PersonUuid uuid

instance ToHttpApiData PersonUuid where
    toUrlPiece (PersonUuid uuid) = UUID.toText uuid

nextRandomPersonUuid :: MonadIO m => m PersonUuid
nextRandomPersonUuid = liftIO $ PersonUuid <$> UUID.nextRandom

personUuidText :: PersonUuid -> Text
personUuidText (PersonUuid uuid) = UUID.toText uuid

parsePersonUuid :: Text -> Maybe PersonUuid
parsePersonUuid = fmap PersonUuid . UUID.fromText

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

newtype NoteIndex = NoteIndex
    { noteIndexList :: [PersonNoteUuid]
    } deriving (Show, Eq, Ord, Generic)

instance FromJSON NoteIndex where
    parseJSON ob =
        (withObject "NoteIndex" $ \o -> NoteIndex <$> o .: "noteIndexList") ob -- TODO remove old JSON parsing
         <|>
        (withArray "NoteIndex" $ \a ->
             (NoteIndex . toList) <$> traverse parseJSON a)
            ob

instance ToJSON NoteIndex where
    toJSON = toJSON . noteIndexList

newNoteIndex :: NoteIndex
newNoteIndex = NoteIndex {noteIndexList = []}

newtype PersonNoteUuid = PersonNoteUuid
    { unPersonNoteUuid :: UUID
    } deriving (Show, Eq, Ord, Generic)

instance Validity PersonNoteUuid where
    isValid = const True

instance FromJSON PersonNoteUuid where
    parseJSON =
        withText "PersonNoteUuid" $ \t ->
            case UUID.fromText t of
                Nothing -> fail "Invalid Text when parsing UUID"
                Just u -> pure $ PersonNoteUuid u

instance ToJSON PersonNoteUuid where
    toJSON (PersonNoteUuid u) = JSON.String $ UUID.toText u

nextRandomPersonNoteUuid :: MonadIO m => m PersonNoteUuid
nextRandomPersonNoteUuid = liftIO $ PersonNoteUuid <$> UUID.nextRandom

personNoteUuidText :: PersonNoteUuid -> Text
personNoteUuidText (PersonNoteUuid uuid) = UUID.toText uuid

personNoteUuidString :: PersonNoteUuid -> String
personNoteUuidString (PersonNoteUuid uuid) = UUID.toString uuid

data PersonNote = PersonNote
    { personNoteContents :: Text
    , personNoteTimestamp :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity PersonNote

instance FromJSON PersonNote where
    parseJSON ob =
        (withObject "PersonNote" $ \o ->
             PersonNote <$> o .: "personNoteContents" <*>
             o .: "personNoteTimestamp")
            ob <|>
        (withObject "PersonNote" $ \o ->
             PersonNote <$> o .: "contents" <*> o .: "timestamp")
            ob

instance ToJSON PersonNote where
    toJSON PersonNote {..} =
        object
            [ "contents" .= personNoteContents
            , "timestamp" .= personNoteTimestamp
            ]

data EditingResult
    = EditingSuccess
    | EditingFailure Text
    deriving (Show, Eq, Generic)

newtype DataSettings = DataSettings
    { dataSetWolfDir :: Path Abs Dir
    } deriving (Show, Eq)
