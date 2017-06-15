{-# LANGUAGE DeriveGeneric #-}

module Wolf.Types where

import Import

import Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID

newtype Index = Index
    { indexMap :: Map String PersonUuid
    } deriving (Show, Eq, Ord, Generic)

instance Validity Index

instance FromJSON Index

instance ToJSON Index

newIndex :: Index
newIndex = Index {indexMap = M.empty}

newtype PersonUuid = PersonUuid
    { unPersonUuid :: UUID
    } deriving (Show, Eq, Ord, Generic)

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

nextRandomPersonUuid :: MonadIO m => m PersonUuid
nextRandomPersonUuid = liftIO $ PersonUuid <$> UUID.nextRandom

personUuidString :: PersonUuid -> String
personUuidString (PersonUuid uuid) = UUID.toString uuid

parsePersonUuid :: String -> Maybe PersonUuid
parsePersonUuid = fmap PersonUuid . UUID.fromString

data PersonEntry = PersonEntry
    { personEntryProperties :: Map String PersonPropertyValue
    , personEntryLastUpdatedTimestamp :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity PersonEntry

instance FromJSON PersonEntry

instance ToJSON PersonEntry

newPersonEntry :: UTCTime -> PersonEntry
newPersonEntry now =
    PersonEntry
    {personEntryProperties = M.empty, personEntryLastUpdatedTimestamp = now}

data PersonPropertyValue = PersonPropertyValue
    { personPropertyValueContents :: String
    , personPropertyValueLastUpdatedTimestamp :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity PersonPropertyValue

instance FromJSON PersonPropertyValue

instance ToJSON PersonPropertyValue

newtype NoteIndex = NoteIndex
    { noteIndexList :: [PersonNoteUuid]
    } deriving (Show, Eq, Ord, Generic)

instance FromJSON NoteIndex

instance ToJSON NoteIndex

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

personNoteUuidString :: PersonNoteUuid -> String
personNoteUuidString (PersonNoteUuid uuid) = UUID.toString uuid

data PersonNote = PersonNote
    { personNoteContents :: Text
    , personNoteTimestamp :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity PersonNote

instance FromJSON PersonNote

instance ToJSON PersonNote

data EditingResult
    = EditingSuccess
    | EditingFailure String
    deriving (Show, Eq, Generic)
