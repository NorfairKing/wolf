{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.NoteIndex.Types where

import Import

import Data.Aeson as JSON
import qualified Data.Set as S
import Data.UUID as UUID
import Data.UUID.V4 as UUID

{-# ANN module ("HLint: ignore Use &&" :: Text) #-}

newtype NoteIndex = NoteIndex
    { noteIndexSet :: Set NoteUuid
    } deriving (Show, Eq, Ord, Generic)

instance Validity NoteIndex

instance NFData NoteIndex

instance FromJSON NoteIndex where
    parseJSON ob =
        (withObject "NoteIndex" $ \o -> NoteIndex <$> o .: "noteIndexList") ob -- TODO remove old JSON parsing
         <|>
        (withArray "NoteIndex" $ \a ->
             (NoteIndex . S.fromList . toList) <$> traverse parseJSON a)
            ob

instance ToJSON NoteIndex where
    toJSON = toJSON . noteIndexSet

newNoteIndex :: NoteIndex
newNoteIndex = NoteIndex {noteIndexSet = S.empty}

newtype NoteUuid = NoteUuid
    { unNoteUuid :: UUID
    } deriving (Show, Eq, Ord, Generic)

instance Validity NoteUuid where
    isValid = const True

instance NFData NoteUuid

instance FromJSON NoteUuid where
    parseJSON =
        withText "NoteUuid" $ \t ->
            case UUID.fromText t of
                Nothing -> fail "Invalid Text when parsing UUID"
                Just u -> pure $ NoteUuid u

instance ToJSON NoteUuid where
    toJSON (NoteUuid u) = JSON.String $ UUID.toText u

nextRandomNoteUuid :: MonadIO m => m NoteUuid
nextRandomNoteUuid = liftIO $ NoteUuid <$> UUID.nextRandom

noteUuidText :: NoteUuid -> Text
noteUuidText (NoteUuid uuid) = UUID.toText uuid

noteUuidString :: NoteUuid -> String
noteUuidString (NoteUuid uuid) = UUID.toString uuid
