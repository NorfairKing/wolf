{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Suggestion.Types
    ( SuggestionUuid
    , SuggestionType(..)
    , Suggestion(..)
    , AliasSuggestion(..)
    , EntrySuggestion(..)
    , sameEntrySuggestionData
    , sameEntrySuggestion
    ) where

import Import

import Data.Aeson

import Wolf.Data.Entry.Types
import Wolf.Data.Index.Types
import Wolf.Data.People.Types

type SuggestionUuid = UUID Sugg

data Sugg

newtype SuggestionType a =
    SuggestionType (Path Rel Dir)
    deriving (Show, Eq, Generic)

data Suggestion a = Suggestion
    { suggestionSuggestor :: Text
    , suggestionReason :: Text
    , suggestionData :: a
    } deriving (Show, Eq, Generic)

instance Validity a => Validity (Suggestion a)

instance NFData a => NFData (Suggestion a)

instance FromJSON a => FromJSON (Suggestion a) where
    parseJSON =
        withObject "Suggestion" $ \o ->
            Suggestion <$> o .: "suggestor" <*> o .: "reason" <*> o .: "data"

instance ToJSON a => ToJSON (Suggestion a) where
    toJSON Suggestion {..} =
        object
            [ "suggestor" .= suggestionSuggestor
            , "reason" .= suggestionReason
            , "data" .= suggestionData
            ]

data AliasSuggestion = AliasSuggestion
    { aliasSuggestionPerson :: PersonUuid
    , aliasSuggestionAlias :: Alias
    } deriving (Show, Eq, Generic)

instance Validity AliasSuggestion

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
    } deriving (Show, Eq, Generic)

instance Validity EntrySuggestion

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
