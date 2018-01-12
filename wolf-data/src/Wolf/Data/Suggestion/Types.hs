{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Suggestion.Types
    ( SuggestionUuid
    , SuggestionType(..)
    , parseSuggestionType
    , SuggestionIndex(..)
    , SuggestionHash(..)
    , Suggestion(..)
    , AliasSuggestion(..)
    , EntrySuggestion(..)
    , sameEntrySuggestionData
    , sameEntrySuggestion
    ) where

import Import

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Time
import Data.Word
import Numeric (showHex)
import qualified System.FilePath as FP
import Text.Read

import Wolf.Data.Entry.Types
import Wolf.Data.Index.Types
import Wolf.Data.People.Types

type SuggestionUuid = UUID Sugg

data Sugg

newtype SuggestionType = SuggestionType
    { suggestionTypePath :: Path Rel Dir
    } deriving (Show, Eq, Ord, Generic)

parseSuggestionType :: FilePath -> Maybe SuggestionType
parseSuggestionType = fmap SuggestionType . parseRelDir

instance Validity SuggestionType where
    validate (SuggestionType p) =
        check
            (length (filter (/= ".") . FP.splitDirectories $ fromRelDir p) == 1)
            "only contains one path component"
    isValid = isValidByValidating

newtype SuggestionIndex a = SuggestionIndex
    { suggestionIndexMap :: Map (SuggestionHash a) SuggestionUuid
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity (SuggestionIndex a)

newtype SuggestionHash a =
    SuggestionHash Word64
    deriving (Show, Eq, Ord, Generic)

instance Validity (SuggestionHash a)

instance FromJSONKey (SuggestionHash a) where
    fromJSONKey = FromJSONKeyTextParser parseSuggestionHashText

instance FromJSON (SuggestionHash a) where
    parseJSON = withText "SuggestionHash" parseSuggestionHashText

parseSuggestionHashText :: Text -> Parser (SuggestionHash a)
parseSuggestionHashText t =
    case readMaybe (T.unpack t) of
        Nothing -> fail "failed to parse SuggestionHash, not a valid int"
        Just i -> pure $ SuggestionHash i

instance ToJSONKey (SuggestionHash a) where
    toJSONKey = toJSONKeyText toJSONTextHash

instance ToJSON (SuggestionHash a) where
    toJSON = JSON.String . toJSONTextHash

toJSONTextHash :: SuggestionHash a -> Text
toJSONTextHash (SuggestionHash i) = T.pack $ "0x" ++ showHex i ""

data Suggestion a = Suggestion
    { suggestionSuggestor :: Text
    , suggestionReason :: Text
    , suggestionTimestamp :: UTCTime
    , suggestionData :: a
    } deriving (Show, Ord, Generic)

instance Validity a => Validity (Suggestion a)

instance Eq a => Eq (Suggestion a) where
    s1 == s2 =
        suggestionSuggestor s1 == suggestionSuggestor s2 &&
        suggestionReason s1 == suggestionReason s2 &&
        suggestionData s1 == suggestionData s2

instance NFData a => NFData (Suggestion a)

instance FromJSON a => FromJSON (Suggestion a) where
    parseJSON =
        withObject "Suggestion" $ \o ->
            Suggestion <$> o .: "suggestor" <*> o .: "reason" <*>
            o .: "timestamp" <*>
            o .: "data"

instance ToJSON a => ToJSON (Suggestion a) where
    toJSON Suggestion {..} =
        object
            [ "suggestor" .= suggestionSuggestor
            , "reason" .= suggestionReason
            , "timestamp" .= suggestionTimestamp
            , "data" .= suggestionData
            ]

instance Functor Suggestion where
    fmap f s = s {suggestionData = f $ suggestionData s}

data AliasSuggestion = AliasSuggestion
    { aliasSuggestionPerson :: PersonUuid
    , aliasSuggestionAlias :: Alias
    } deriving (Show, Eq, Ord, Generic)

instance Validity AliasSuggestion

instance Hashable AliasSuggestion

instance NFData AliasSuggestion

instance FromJSON AliasSuggestion where
    parseJSON =
        withObject "AliasSuggestion" $ \o ->
            AliasSuggestion <$> o .: "person" <*> o .: "alias"

instance ToJSON AliasSuggestion where
    toJSON AliasSuggestion {..} =
        object
            ["person" .= aliasSuggestionPerson, "alias" .= aliasSuggestionAlias]

data EntrySuggestion = EntrySuggestion
    { entrySuggestionEntry :: PersonEntry
    , entrySuggestionNewAliases :: [Alias]
    , entrySuggestionLikelyRelevantPerson :: Maybe (PersonUuid, Double)
    } deriving (Show, Eq, Ord, Generic)

instance Validity EntrySuggestion

instance Hashable EntrySuggestion

instance NFData EntrySuggestion

instance FromJSON EntrySuggestion where
    parseJSON =
        withObject "EntrySuggestion" $ \o ->
            EntrySuggestion <$> o .: "entry" <*> o .: "new-aliases" <*>
            o .: "relevant-person"

instance ToJSON EntrySuggestion where
    toJSON EntrySuggestion {..} =
        object
            [ "entry" .= entrySuggestionEntry
            , "new-aliases" .= entrySuggestionNewAliases
            , "relevant-person" .= entrySuggestionLikelyRelevantPerson
            ]

sameEntrySuggestionData ::
       Suggestion EntrySuggestion -> Suggestion EntrySuggestion -> Bool
sameEntrySuggestionData =
    aaand
        [ (==) `on` suggestionSuggestor
        , (==) `on` suggestionReason
        , sameEntrySuggestion `on` suggestionData
        ]

sameEntrySuggestion :: EntrySuggestion -> EntrySuggestion -> Bool
sameEntrySuggestion =
    aaand
        [ sameValues `on` (personEntryProperties . entrySuggestionEntry)
        , (==) `on` entrySuggestionNewAliases
        , (==) `on` entrySuggestionLikelyRelevantPerson
        ]

aaand :: [a -> a -> Bool] -> a -> a -> Bool
aaand fs a b = all (\f -> f a b) fs
