{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.NoteIndex.Types where

import Import

import Data.Aeson as JSON
import Data.Aeson.Types as JSON
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
        (withArray "NoteIndex" $
         fmap (NoteIndex . S.fromList . toList) . traverse parseJSON)
            ob

instance ToJSON NoteIndex where
    toJSON = toJSON . noteIndexSet

newNoteIndex :: NoteIndex
newNoteIndex = NoteIndex {noteIndexSet = S.empty}

-- | Check if a given note index contains a given note uuid
containsNoteUuid :: NoteIndex -> NoteUuid -> Bool
containsNoteUuid noteIndex noteUuid = noteUuid `S.member` noteIndexSet noteIndex

isSubNoteIndexOf :: NoteIndex -> NoteIndex -> Bool
isSubNoteIndexOf ni1 ni2 = all (containsNoteUuid ni2) (noteIndexSet ni1)

newtype NoteUuid = NoteUuid
    { unNoteUuid :: UUID
    } deriving (Show, Eq, Ord, Generic)

instance Validity NoteUuid where
    isValid = const True

instance NFData NoteUuid

instance FromJSONKey NoteUuid where
    fromJSONKey = FromJSONKeyTextParser textJSONParseNoteUUID

instance ToJSONKey NoteUuid where
    toJSONKey = toJSONKeyText (UUID.toText . unNoteUuid)

instance FromJSON NoteUuid where
    parseJSON = jsonParseNoteUUID

instance ToJSON NoteUuid where
    toJSON (NoteUuid u) = JSON.String $ UUID.toText u

jsonParseNoteUUID :: Value -> Parser NoteUuid
jsonParseNoteUUID = withText "NoteUuid" textJSONParseNoteUUID

textJSONParseNoteUUID :: Text -> Parser NoteUuid
textJSONParseNoteUUID t =
    case UUID.fromText t of
        Nothing -> fail "Invalid Text when parsing UUID"
        Just u -> pure $ NoteUuid u

nextRandomNoteUuid :: MonadIO m => m NoteUuid
nextRandomNoteUuid = liftIO $ NoteUuid <$> UUID.nextRandom

noteUuidText :: NoteUuid -> Text
noteUuidText (NoteUuid uuid) = UUID.toText uuid

noteUuidString :: NoteUuid -> String
noteUuidString (NoteUuid uuid) = UUID.toString uuid
