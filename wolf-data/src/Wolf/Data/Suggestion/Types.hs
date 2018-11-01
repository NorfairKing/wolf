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
    , hashSuggestion
    , Suggestion(..)
    , AliasSuggestion(..)
    , EntrySuggestion(..)
    , sameEntrySuggestionData
    , sameEntrySuggestion
    , SuggestionRepo(..)
    , SuggestionTypeRepo(..)
    , Agreement(..)
    , parseAgreement
    , renderAgreement
    ) where

import Import

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.Types
import qualified Data.Map as M
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
parseSuggestionType fp = SuggestionType <$> parseRelDir fp >>= constructValid

instance Validity SuggestionType where
    validate (SuggestionType p) =
        mconcat
            [ annotate p "suggestionTypePath"
            , check
                  (length (filter (/= ".") . FP.splitDirectories $ fromRelDir p) ==
                   1)
                  "only contains one path component"
            ]

instance NFData SuggestionType

instance FromJSON SuggestionType where
    parseJSON = withText "SuggestionType" parseSuggestionTypeText

instance FromJSONKey SuggestionType where
    fromJSONKey = FromJSONKeyTextParser parseSuggestionTypeText

parseSuggestionTypeText :: Text -> Parser SuggestionType
parseSuggestionTypeText t =
    case parseRelDir $ T.unpack t of
        Nothing ->
            fail "failed to parse SuggestionType, not a valid Path Rel Dir"
        Just rd -> pure $ SuggestionType rd

instance ToJSON SuggestionType where
    toJSON = JSON.String . toJSONTextType

instance ToJSONKey SuggestionType where
    toJSONKey = toJSONKeyText toJSONTextType

toJSONTextType :: SuggestionType -> Text
toJSONTextType (SuggestionType p) = T.pack $ fromRelDir p

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

hashSuggestion :: Hashable a => Suggestion a -> SuggestionHash a
hashSuggestion = SuggestionHash . fromIntegral . hash . suggestionData

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
    } deriving (Show, Ord, Eq, Generic)

instance Validity a => Validity (Suggestion a)

instance Hashable a => Hashable (Suggestion a)

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

newtype SuggestionRepo = SuggestionRepo
    { allSuggestions :: Map SuggestionType (SuggestionTypeRepo JSON.Value)
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity SuggestionRepo

instance NFData SuggestionRepo

data SuggestionTypeRepo a = SuggestionTypeRepo
    { suggestionTypeRepoUnused :: Map SuggestionUuid (Suggestion a)
    , suggestionTypeRepoUsed :: Map SuggestionUuid (Suggestion a)
    } deriving (Show, Eq, Ord, Generic)

instance (Validity a, Hashable a) => Validity (SuggestionTypeRepo a) where
    validate SuggestionTypeRepo {..} =
        mconcat
            [ annotate suggestionTypeRepoUnused "suggestionsTypeRepoUnused"
            , annotate suggestionTypeRepoUsed  "suggestionsTypeRepoUsed"
            , check
                  (M.size (M.map hashSuggestion suggestionTypeRepoUnused) ==
                   M.size suggestionTypeRepoUnused)
                  "contains no duplicate unused suggestion data"
            , check
                  (M.size (M.map hashSuggestion suggestionTypeRepoUsed) ==
                   M.size suggestionTypeRepoUsed)
                  "contains no duplicate used suggestion data"
            , check
                  (M.null
                       (M.intersection
                            suggestionTypeRepoUnused
                            suggestionTypeRepoUsed))
                  "contains no duplicate uuids"
            ]

instance NFData a => NFData (SuggestionTypeRepo a)

instance FromJSON a => FromJSON (SuggestionTypeRepo a) where
    parseJSON =
        withObject "SuggestionTypeRepo" $ \o ->
            SuggestionTypeRepo <$> o .: "unused" <*> o .: "used"

instance ToJSON a => ToJSON (SuggestionTypeRepo a) where
    toJSON SuggestionTypeRepo {..} =
        object
            [ "unused" .= suggestionTypeRepoUnused
            , "used" .= suggestionTypeRepoUsed
            ]

data Agreement
    = Agree
    | Disagree
    deriving (Show, Eq, Generic)

parseAgreement :: Text -> Either Text Agreement
parseAgreement "yes" = Right Agree
parseAgreement "no" = Right Disagree
parseAgreement t = Left $ "Invalid 'Agreement' value: " <> t

renderAgreement :: Agreement -> Text
renderAgreement Agree = "yes"
renderAgreement Disagree = "no"
