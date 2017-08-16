{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Types where

import Import

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

data EditingResult
    = EditingSuccess
    | EditingFailure Text
    deriving (Show, Eq, Generic)

newtype DataSettings = DataSettings
    { dataSetWolfDir :: Path Abs Dir
    } deriving (Show, Eq)
