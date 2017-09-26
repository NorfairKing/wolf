{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.People.Types
    ( PersonUuid(..)
    , personUuidBs
    , personUuidLBs
    , personUuidString
    , personUuidText
    , nextRandomPersonUuid
    , parsePersonUuid
    , parsePersonUuidString
    ) where

import Import

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Servant
import Text.Read

newtype PersonUuid = PersonUuid
    { unPersonUuid :: UUID
    } deriving (Eq, Ord, Generic)

instance Show PersonUuid where
    show (PersonUuid u) = show u

instance Read PersonUuid where
    readPrec = PersonUuid <$> readPrec

personUuidBs :: PersonUuid -> ByteString
personUuidBs (PersonUuid uuid) = UUID.toASCIIBytes uuid

personUuidLBs :: PersonUuid -> LB.ByteString
personUuidLBs = LB.fromStrict . personUuidBs

personUuidString :: PersonUuid -> String
personUuidString (PersonUuid uuid) = UUID.toString uuid

personUuidText :: PersonUuid -> Text
personUuidText (PersonUuid uuid) = UUID.toText uuid

nextRandomPersonUuid :: MonadIO m => m PersonUuid
nextRandomPersonUuid = liftIO $ PersonUuid <$> UUID.nextRandom

parsePersonUuid :: Text -> Maybe PersonUuid
parsePersonUuid = fmap PersonUuid . UUID.fromText

parsePersonUuidString :: String -> Maybe PersonUuid
parsePersonUuidString = fmap PersonUuid . UUID.fromString

instance Validity PersonUuid where
    isValid = const True

instance NFData PersonUuid

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
