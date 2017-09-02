{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Entry.Internal where

import Import

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Catch
import Data.Conduit
import qualified Data.Text as T
import Data.Time
import Data.Yaml.Builder as Yaml
import Data.Yaml.Parser as Yaml
import Text.Libyaml as Yaml

import Control.Monad.Trans.Resource

import Wolf.Data

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

parseFirstnameLastname :: Text -> Maybe (Text, Text)
parseFirstnameLastname s =
    case T.words s of
        [fn, ln] -> Just (fn, ln)
        _ -> Nothing

tmpEntryFileContents :: PersonEntry -> ByteString
tmpEntryFileContents pe = Yaml.toByteString $ ForEditor pe

toRawYaml :: PersonProperty -> RawYaml
toRawYaml (PVal c) = RVal $ personPropertyValueContents c
toRawYaml (PList ls) = RList $ map toRawYaml ls
toRawYaml (PMap tups) = RMap $ map (second toRawYaml) tups

newtype ForEditor a =
    ForEditor a
    deriving (Show, Eq, Generic)

instance Validity a => Validity (ForEditor a)

instance ToYaml (ForEditor PersonEntry) where
    toYaml (ForEditor pe) = go (personEntryProperties pe)
      where
        go :: PersonProperty -> YamlBuilder
        go (PVal ppv) = Yaml.string $ personPropertyValueContents ppv
        go (PList ls) = Yaml.array $ map go ls
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
    | RList [RawYaml]
    | RMap [(Text, RawYaml)]
    deriving (Show, Eq, Generic)

instance Validity RawYaml where
    isValid (RVal t) = isValid t
    isValid (RList ls) = isValid ls
    isValid (RMap []) = False
    isValid (RMap tups) =
        isValid tups &&
        (let ls = map fst tups
         in nub ls == ls)

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
