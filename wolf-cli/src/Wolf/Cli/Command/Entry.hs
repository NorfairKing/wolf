{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Entry where

import Import

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Catch
import qualified Data.ByteString as SB
import Data.Conduit
import qualified Data.Text as T
import Data.Time
import Data.Yaml.Builder as Yaml
import Data.Yaml.Parser as Yaml
import Text.Libyaml as Yaml

import Control.Monad.Trans.Resource

import Wolf.Cli.Editor
import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data
import Wolf.Data.Git

entry :: (MonadIO m, MonadReader Settings m) => Text -> m ()
entry person =
    runData $
    withInitCheck $ do
        origIndex <- getIndexWithDefault
        (personUuid, index) <- lookupOrCreateNewPerson person origIndex
        tmpFile <- tmpPersonEntryFile personUuid
        mPersonEntry <- getPersonEntry personUuid
        (origPersonEntry, inFilePersonEntry) <-
            case mPersonEntry of
                Nothing -> do
                    now <- liftIO getCurrentTime
                    pure $
                        (,) newPersonEntry $
                        fromMaybe newPersonEntry $ do
                            (fn, ln) <- parseFirstnameLastname person
                            personEntry $
                                PMap
                                    [ ( "first name"
                                      , PVal $ PersonPropertyValue fn now)
                                    , ( "last name"
                                      , PVal $ PersonPropertyValue ln now)
                                    ]
                Just pe -> pure (pe, pe)
        ensureDir $ parent tmpFile
        let tmpFileContents = tmpEntryFileContents inFilePersonEntry
        liftIO $ SB.writeFile (toFilePath tmpFile) tmpFileContents
        editResult <- startEditorOn tmpFile
        case editResult of
            EditingFailure reason ->
                liftIO $
                die $
                unwords
                    [ "ERROR: failed to edit the note file:"
                    , show reason
                    , ",not saving."
                    ]
            EditingSuccess -> do
                contents <- liftIO $ SB.readFile $ toFilePath tmpFile
                case parseEntryFileContents contents of
                    Left err ->
                        liftIO $
                        die $ "Unable to parse entry file: " <> show err
                    Right (ForEditor personEntryMap) -> do
                        now <- liftIO getCurrentTime
                        case reconstructPersonEntry
                                 now
                                 origPersonEntry
                                 personEntryMap of
                            Nothing ->
                                liftIO $
                                die $
                                unwords
                                    [ "Failed to reconstruct a person entry: edit resulted in an invalid person entry"
                                    ]
                            Just pe ->
                                unless (pe == origPersonEntry) $ do
                                    putPersonEntry personUuid pe
                                    putIndex index
                                    makeGitCommit $
                                        unwords
                                            [ "Added/changed entry for"
                                            , T.unpack person
                                            ]

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
    fillWithNow (RMap tups) = PMap $ map (second fillWithNow) tups
    go (PVal vo) (RVal vn) =
        PVal
            PersonPropertyValue
            { personPropertyValueContents = vn
            , personPropertyValueLastUpdatedTimestamp =
                  if vn == personPropertyValueContents vo
                      then personPropertyValueLastUpdatedTimestamp vo
                      else now
            }
    go (PMap _) r@(RVal _) = fillWithNow r
    go (PVal _) r@(RMap _) = fillWithNow r
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

parseFirstnameLastname :: Text -> Maybe (Text, Text)
parseFirstnameLastname s =
    case T.words s of
        [fn, ln] -> Just (fn, ln)
        _ -> Nothing

tmpEntryFileContents :: PersonEntry -> ByteString
tmpEntryFileContents pe = Yaml.toByteString $ ForEditor pe

toRawYaml :: PersonProperty -> RawYaml
toRawYaml (PVal c) = RVal $ personPropertyValueContents c
toRawYaml (PMap tups) = RMap $ map (second toRawYaml) tups

newtype ForEditor a =
    ForEditor a

instance ToYaml (ForEditor PersonEntry) where
    toYaml (ForEditor pe) = go (personEntryProperties pe)
      where
        go :: PersonProperty -> YamlBuilder
        go (PVal ppv) = Yaml.string $ personPropertyValueContents ppv
        go (PMap tups) = Yaml.mapping $ map (second go) tups

parseEntryFileContents ::
       ByteString -> Either YamlParseException (ForEditor RawYaml)
parseEntryFileContents bs =
    unsafePerformIO $ do
        rawDoc <- runResourceT $ Yaml.decode bs $$ Yaml.sinkRawDoc
        (Right <$> parseRawDoc rawDoc) `catch` (pure . Left)

{-# NOINLINE parseEntryFileContents #-}
data RawYaml
    = RVal Text
    | RMap [(Text, RawYaml)]
    deriving (Show, Eq, Generic)

instance Validity RawYaml where
    isValid (RVal t) = isValid t
    isValid (RMap tups) =
        isValid tups &&
        (let ls = map fst tups
         in nub ls == ls)

instance FromYaml (ForEditor RawYaml) where
    fromYaml yv = ForEditor <$> (parseRVal yv <|> parseRMap yv)
      where
        parseRVal = withText "RVal" (pure . RVal)
        parseRMap =
            withMapping "RMap" $ \tups ->
                fmap RMap $
                forM tups $ \(k, v) -> do
                    (ForEditor ry) <- fromYaml v
                    pure (k, ry)

stripWhitespace :: String -> String
stripWhitespace = reverse . dropWhite . reverse . dropWhite
  where
    dropWhite = dropWhile (\c -> c == ' ' || c == '\t')

tmpPersonEntryFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> m (Path Abs File)
tmpPersonEntryFile personUuid = do
    td <- liftIO getTempDir
    liftIO $
        resolveFile td $ T.unpack (personUuidText personUuid) ++ "-entry.yaml"
