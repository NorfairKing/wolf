{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Entry where

import Import

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Time

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
                            personEntry
                                [ ("first name", PersonPropertyValue fn now)
                                , ("last name", PersonPropertyValue ln now)
                                ]
                Just pe -> pure (pe, pe)
        ensureDir $ parent tmpFile
        let tmpFileContents =
                tmpEntryFileContents person personUuid inFilePersonEntry
        liftIO $ T.writeFile (toFilePath tmpFile) tmpFileContents
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
                contents <- liftIO $ T.readFile $ toFilePath tmpFile
                case parseEntryFileContents contents of
                    Left err ->
                        liftIO $
                        die $
                        unwords ["Unable to parse entry file:", T.unpack err]
                    Right personEntryMap -> do
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

reconstructPersonEntry ::
       UTCTime -> PersonEntry -> [(Text, Text)] -> Maybe PersonEntry
reconstructPersonEntry now old newMap =
    if map (second personPropertyValueContents) (personEntryTuples old) ==
       newMap
        then Just old -- If there is no difference, don't change the last changed timestamp.
        else personEntry $
             map (\(k, v) -> (k, go k v)) $ nubBy ((==) `on` fst) newMap
  where
    go :: Text -> Text -> PersonPropertyValue
    go key value =
        case lookup key (personEntryTuples old) of
            Nothing -- Key did not exist before, therefore it was created here.
             ->
                PersonPropertyValue
                { personPropertyValueContents = value
                , personPropertyValueLastUpdatedTimestamp = now
                }
            Just oldValue -- Key did exist before, have to check if there's a difference.
             ->
                PersonPropertyValue
                { personPropertyValueContents = value
                , personPropertyValueLastUpdatedTimestamp =
                      if value == personPropertyValueContents oldValue
                          then personPropertyValueLastUpdatedTimestamp oldValue
                          else now
                }

parseFirstnameLastname :: Text -> Maybe (Text, Text)
parseFirstnameLastname s =
    case T.words s of
        [fn, ln] -> Just (fn, ln)
        _ -> Nothing

tmpEntryFileContents :: Text -> PersonUuid -> PersonEntry -> Text
tmpEntryFileContents person personUuid pe =
    T.unlines $
    map (uncurry toLineStr) (personEntryTuples pe) ++
    separator ++
    map (uncurry toLineStr')
        [("uuid", personUuidText personUuid), ("reference used", person)]
  where
    separator = ["", "", "", line, str, line]
      where
        str = "| Anything below this bar will be ignored. |"
        line = T.pack $ replicate (T.length str) '-'
    toLineStr k v = toLineStr' k $ personPropertyValueContents v
    toLineStr' k v = T.unwords [k <> ":", v]

parseEntryFileContents :: Text -> Either Text [(Text, Text)]
parseEntryFileContents str =
    mapM
        parseProperty
        (filter (not . T.null) . takeWhile (not . T.isPrefixOf "---") . T.lines $
         str)
  where
    parseProperty :: Text -> Either Text (Text, Text)
    parseProperty s =
        case break (== ':') $ T.unpack s of
            (_, []) ->
                Left $
                T.pack $ unwords ["Could not parse a property from", show s]
            (key, ':':val) ->
                Right
                    (T.pack $ stripWhitespace key, T.pack $ stripWhitespace val)
            _ ->
                Left $
                T.pack $
                unwords
                    [ "Something really weird happened while parsing"
                    , show s
                    , "for a property"
                    ]

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
        resolveFile td $ T.unpack (personUuidText personUuid) ++ "-entry.wolf"
