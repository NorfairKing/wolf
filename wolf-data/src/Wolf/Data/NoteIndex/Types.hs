{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.NoteIndex.Types where

import Import

import Data.Aeson as JSON
import qualified Data.Set as S

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

type NoteUuid = UUID NoteIndex
