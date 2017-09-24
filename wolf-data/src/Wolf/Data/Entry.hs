{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Wolf.Data.Entry
    ( PersonEntry
    , personEntry
    , personEntryProperties
    , sameProperties
    , newPersonEntry
    , PersonProperty(..)
    , sameValues
    , PersonPropertyValue(..)
    , sameContents
    , entryContents
    , entryContentsBS
    , updatePersonEntry
    , UpdateResult(..)
    , EntryParseException
    , prettyPrintEntryParseException
    ) where

import Import

import System.IO.Unsafe (unsafePerformIO)

import Control.Exception (evaluate)
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import qualified Data.ByteString as SB
import Data.Conduit
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Yaml as Yaml
       (ParseException(..), prettyPrintParseException)
import Data.Yaml.Builder as Yaml
import Data.Yaml.Parser as Yaml
import Text.Libyaml as Yaml

import Wolf.Data.Entry.Types

entryContents :: PersonEntry -> Text
entryContents pe =
    case TE.decodeUtf8' $ entryContentsBS pe of
        Left err ->
            "Error: Unable to transform entry to text, decoding ByteString to Text failed: " <>
            T.pack (show err) -- Should never happen.
        Right t -> t

entryContentsBS :: PersonEntry -> ByteString
entryContentsBS = Yaml.toByteString . ForEditor

updatePersonEntry ::
       UTCTime -> PersonEntry -> ByteString -> UpdateResult PersonEntry
updatePersonEntry now old bs =
    if SB.null bs
        then UpdateWasDeletion
        else case parseEntryFileContents bs of
                 Left ye -> UpdateParseFailure ye
                 Right (ForEditor ry) ->
                     case reconstructPersonEntry now old ry of
                         Nothing -> UpdateValidityFailure
                         Just pe ->
                             if pe == old
                                 then UpdateUnchanged
                                 else UpdateSuccess pe

data UpdateResult a
    = UpdateSuccess a
    | UpdateUnchanged
    | UpdateParseFailure EntryParseException
    | UpdateValidityFailure
    | UpdateWasDeletion
    deriving (Show, Generic)

instance Validity a => Validity (UpdateResult a) where
    isValid (UpdateSuccess a) = isValid a
    isValid _ = True
    validate (UpdateSuccess a) = a <?!> "UpdateSuccess"
    validate ur = validateByCheckingName "UpdateResult" ur

data EntryParseException
    = EntryYamlParseException YamlParseException
    | EntryYamlException YamlException
    | EntryParseException ParseException
    deriving (Show, Generic)

prettyPrintEntryParseException :: EntryParseException -> String
prettyPrintEntryParseException (EntryYamlParseException ype) = show ype
prettyPrintEntryParseException (EntryYamlException ye) = show ye
prettyPrintEntryParseException (EntryParseException ye) =
    prettyPrintParseException ye

reconstructPersonEntry :: UTCTime -> PersonEntry -> RawYaml -> Maybe PersonEntry
reconstructPersonEntry now old new =
    let oldProp = personEntryProperties old
    in personEntry $ go oldProp new
  where
    fillWithNow :: RawYaml -> PersonProperty
    fillWithNow (RVal n) =
        PVal
            PersonPropertyValue
            { personPropertyValueContents = n
            , personPropertyValueLastUpdatedTimestamp = now
            }
    fillWithNow (RList ns) = PList $ map fillWithNow ns
    fillWithNow (RMap tups) = PMap $ map (second fillWithNow) tups
    go :: PersonProperty -> RawYaml -> PersonProperty
    go (PVal vo) (RVal vn) =
        PVal
            PersonPropertyValue
            { personPropertyValueContents = vn
            , personPropertyValueLastUpdatedTimestamp =
                  if vn == personPropertyValueContents vo
                      then personPropertyValueLastUpdatedTimestamp vo
                      else now
            }
    go (PList ol) (RList nl) =
        PList $ zipWith f (map Just ol ++ repeat Nothing) nl
      where
        f :: Maybe PersonProperty -> RawYaml -> PersonProperty
        f mov nv =
            case mov of
                Nothing -> fillWithNow nv
                Just ov -> go ov nv
    go (PMap om) (RMap nm) =
        PMap $ map (\(k, v) -> (k, foo k v)) $ nubBy ((==) `on` fst) nm
      where
        foo :: Text -> RawYaml -> PersonProperty
        foo key value =
            case lookup key om of
                Nothing -- Key did not exist before, therefore it was created here.
                 -> fillWithNow value
                Just oldValue -- Key existed before, have to recurse.
                 -> go oldValue value
    go _ r = fillWithNow r

newtype ForEditor a =
    ForEditor a
    deriving (Show, Eq, Generic)

instance ToYaml (ForEditor PersonEntry) where
    toYaml (ForEditor pe) = go (personEntryProperties pe)
      where
        go :: PersonProperty -> YamlBuilder
        go (PVal ppv) = Yaml.string $ personPropertyValueContents ppv
        go (PList ls) = Yaml.array $ map go ls
        go (PMap tups) = Yaml.mapping $ map (second go) tups

parseEntryFileContents ::
       ByteString -> Either EntryParseException (ForEditor RawYaml)
parseEntryFileContents bs =
    unsafePerformIO $
    let parse = do
            rawDoc <- runResourceT $ Yaml.decode bs $$ Yaml.sinkRawDoc
            parseRawDoc rawDoc >>= (evaluate . force)
    in (Right `liftM` parse) `catches`
       [ Handler $ pure . Left . EntryYamlParseException
       , Handler $ pure . Left . EntryYamlException
       , Handler $ pure . Left . EntryParseException
       ]

{-# NOINLINE parseEntryFileContents #-}
data RawYaml
    = RVal Text
    | RList [RawYaml]
    | RMap [(Text, RawYaml)]
    deriving (Show, Eq, Generic)

instance NFData RawYaml

instance NFData a => NFData (ForEditor a)

instance FromYaml (ForEditor RawYaml) where
    fromYaml yv =
        ForEditor <$> (parseRVal yv <|> parseRList yv <|> parseRMap yv)
      where
        parseRVal = withText "RVal" (pure . RVal)
        parseRList =
            withSequence "RList" $ \vs ->
                fmap RList $
                forM vs $ \y -> do
                    ForEditor ry <- fromYaml y
                    pure ry
        parseRMap =
            withMapping "RMap" $ \tups ->
                fmap RMap $
                forM tups $ \(k, v) -> do
                    (ForEditor ry) <- fromYaml v
                    pure (k, ry)
