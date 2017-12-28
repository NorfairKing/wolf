{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wolf.Web.Server.Handler.People where

import Import

import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import Control.Monad.Reader

import Yesod
import Yesod.Auth

import Wolf.Data

import Wolf.Web.Server.Foundation

getPeopleR :: Handler Html
getPeopleR = do
    void requireAuthId
    pcs <- runData getAllPeopleCards
    withNavBar $ do
        setTitle "Wolf People"
        $(widgetFile "people")

getAllPeopleCards :: ReaderT DataSettings IO WolfWidget
getAllPeopleCards = do
    pm <-
        do ix <- getIndexWithDefault
           M.traverseWithKey (\puuid as -> (,) as <$> getPersonEntry puuid) $
               reverseIndex ix
    pure $ peopleCards pm

peopleCards :: Map PersonUuid (Set Alias, Maybe PersonEntry) -> WolfWidget
peopleCards trips = do
    let pcs = map snd $ sortOn fst $ M.elems $ M.mapWithKey personCard trips
    $(widgetFile "people/items")

personCard :: PersonUuid -> (Set Alias, Maybe PersonEntry) -> (Text, WolfWidget)
personCard uuid (aliases, mpe) =
    let alias =
            case S.toList aliases of
                [] -> "Unaliased person"
                (a:_) -> a
        displayName =
            fromMaybe (aliasText alias) $
            mpe >>= fromEntry >>= renderDisplayName
        mmet = metText <$> (mpe >>= fromEntry)
        mgender = mpe >>= fromEntry
    in (displayName, $(widgetFile "people/item"))

class FromProperty a where
    fromProperty :: PersonProperty -> Maybe a

instance FromProperty Text where
    fromProperty (PVal ppv) = Just $ personPropertyValueContents ppv
    fromProperty _ = Nothing

atKey :: FromProperty a => PersonProperty -> Text -> Maybe a
atKey (PMap tups) key = lookup key tups >>= fromProperty
atKey _ _ = Nothing

fromEntry :: FromProperty a => PersonEntry -> Maybe a
fromEntry = fromProperty . personEntryProperties

data DisplayName = DisplayName
    { displayNameFirstName :: Maybe Text
    , displayNameMiddleName :: Maybe Text
    , displayNameLastName :: Maybe Text
    } deriving (Show, Eq, Generic)

instance FromProperty DisplayName where
    fromProperty pp =
        Just
            DisplayName
            { displayNameFirstName = pp `atKey` "first name"
            , displayNameMiddleName = pp `atKey` "middle name"
            , displayNameLastName = pp `atKey` "last name"
            }

renderDisplayName :: DisplayName -> Maybe Text
renderDisplayName DisplayName {..} =
    case (displayNameFirstName, displayNameLastName) of
        (Nothing, Nothing) -> Nothing
        (Just fn, Nothing) ->
            Just $
            T.unwords $
            fn : maybeToList ((("(" <>) . (<> ")")) <$> displayNameMiddleName)
        (Nothing, Just ln) -> Just $ "Mr or Ms. " <> ln
        (Just fn, Just ln) ->
            Just $ T.unwords $ [fn] ++ maybeToList displayNameMiddleName ++ [ln]

newtype Met = Met
    { metText :: Text
    } deriving (Show, Eq, Generic)

instance FromProperty Met where
    fromProperty pp = Met <$> pp `atKey` "met"

data Gender
    = Male
    | Female
    | Other Text
    deriving (Show, Eq, Generic)

instance FromProperty Gender where
    fromProperty pp =
        flip fmap (pp `atKey` "gender") $ \gt ->
            case gt of
                "male" -> Male
                "female" -> Female
                _ -> Other gt
