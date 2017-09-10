{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Suggestion.Review
    ( reviewSuggestion
    ) where

import Import

import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time

import System.Console.ANSI as ANSI

import Wolf.Data
import Wolf.Data.Git

import Wolf.Cli.Command.Entry.Internal
       (ForEditor(..), parseEntryFileContents, reconstructPersonEntry,
        tmpEntryFileContents)
import Wolf.Cli.Command.Suggestion.Internal
       (renderEntrySuggestion, renderSuggestion)
import Wolf.Cli.Editor
import Wolf.Cli.OptParse.Types
import Wolf.Cli.Prompt
import Wolf.Cli.Report
import Wolf.Cli.Utils

reviewSuggestion :: (MonadIO m, MonadReader Settings m) => m ()
reviewSuggestion = do
    sugs <- runData readPersonEntrySuggestions
    case sugs of
        [] -> liftIO $ putStrLn "No suggestions to review."
        (sug:_) -> reviewSingle sug

reviewSingle ::
       (MonadIO m, MonadReader Settings m) => Suggestion EntrySuggestion -> m ()
reviewSingle s = do
    showData s
    yn <-
        liftIO $
        promptYesNo
            Yes
            "Would you like to act on this suggestion (y) or throw it away (n)?"
    oldAliases <-
        case entrySuggestionLikelyRelevantPerson $ suggestionData s of
            Nothing -> pure []
            Just (uuid, _) -> do
                index <- runData getIndexWithDefault
                pure $ reverseIndexLookup uuid index
    case yn of
        No ->
            runData $ do
                recordUsedPersonEntrySuggestions [s]
                let message =
                        unwords
                            [ "Threw away a suggestion"
                            , case oldAliases ++
                                   entrySuggestionNewAliases (suggestionData s) of
                                  [] -> "for a person without aliases."
                                  (a:_) -> "for " ++ T.unpack a ++ "."
                            ]
                makeGitCommit message
        Yes -> do
            newAliases <-
                promptAboutAliases $
                entrySuggestionNewAliases $ suggestionData s
            when (null $ oldAliases ++ newAliases) $
                liftIO $
                die
                    "Cannot have a person without an index, choose at least one alias."
            mergedEntry <- getMergedEntry s >>= promptAboutEntry
            commitMessage <-
                case entrySuggestionLikelyRelevantPerson $ suggestionData s of
                    Nothing ->
                        runData $ do
                            let displayName =
                                    case newAliases of
                                        [] -> "an un-aliased person"
                                        (a:_) -> a
                            origIndex <- getIndexWithDefault
                            mNewVersion <- createNewPerson newAliases origIndex
                            case mNewVersion of
                                Nothing ->
                                    liftIO $
                                    die
                                        "Unable to add this person, one of the chosen aliases was already asigned."
                                Just (personUuid, index) -> do
                                    case mergedEntry of
                                        Nothing -> pure ()
                                        Just e -> putPersonEntry personUuid e
                                    putIndex index
                                    pure $
                                        unwords
                                            [ "Added entry for"
                                            , T.unpack displayName
                                            , "via a suggestion from"
                                            , T.unpack $ suggestionSuggestor s
                                            ]
                    Just (personUuid, _) ->
                        runData $ do
                            origIndex <- getIndexWithDefault
                            let displayName =
                                    case reverseIndexLookup personUuid origIndex ++
                                         newAliases of
                                        [] -> "an un-aliased person"
                                        (a:_) -> a
                            case addAliases newAliases personUuid origIndex of
                                Nothing ->
                                    liftIO $
                                    die
                                        "Unable to add aliases to this person, one of the chosen new aliases was already asigned."
                                Just index -> do
                                    case mergedEntry of
                                        Nothing -> pure ()
                                        Just e -> putPersonEntry personUuid e
                                    putIndex index
                                    pure $
                                        unwords
                                            [ "Changed entry for"
                                            , T.unpack displayName
                                            , "via a suggestion from"
                                            , T.unpack $ suggestionSuggestor s
                                            ]
            runData $ do
                recordUsedPersonEntrySuggestions [s]
                makeGitCommit commitMessage

showData ::
       (MonadIO m, MonadReader Settings m) => Suggestion EntrySuggestion -> m ()
showData s@Suggestion {..} = do
    let EntrySuggestion {..} = suggestionData
    let white = colored [SetColor Foreground Dull White]
    relevantPersonReport <-
        case entrySuggestionLikelyRelevantPerson of
            Nothing -> pure $ white "No relevant person found."
            Just (uuid, score) -> do
                index <- runData getIndexWithDefault
                let relevantAliases = reverseIndexLookup uuid index
                mentry <- runData $ getPersonEntry uuid
                pure $
                    unlinesReport $
                    [ white "Relevant person:"
                    , stringReport $
                      case relevantAliases of
                          [] -> "No alias found for this person."
                          (a:_) -> T.unpack a
                    , "Score: " <> stringReport (show score)
                    ] ++
                    let yellow = colored [SetColor Foreground Dull Yellow]
                    in case mentry of
                           Nothing -> [yellow "No entry found."]
                           Just entry ->
                               [ yellow "Entry: "
                               , case TE.decodeUtf8' $
                                      tmpEntryFileContents entry of
                                     Left err ->
                                         colored [SetColor Foreground Dull Red] $
                                         "Failed to decode entry UTF8 to Text: " ++
                                         show err
                                     Right r -> stringReport $ T.unpack r
                               ]
    let infoReport =
            unlinesReport
                [ white "Suggestion:"
                , renderSuggestion renderEntrySuggestion s
                , relevantPersonReport
                ]
    liftIO $ putStr $ renderReport infoReport

promptAboutAliases :: (MonadIO m, MonadReader Settings m) => [Text] -> m [Text]
promptAboutAliases aliases = do
    yn <-
        liftIO $
        promptYesNo
            No
            "Do you want to use these _new_ aliases as-is for this suggestion (y) or edit them (n)?"
    case yn of
        Yes -> pure aliases
        No -> do
            taf <- tmpAliasFile
            liftIO $
                T.writeFile (toFilePath taf) $
                T.unlines $
                aliases ++
                [ ""
                , "# Leave one alias per line."
                , "# Lines beginning with a '#' will be ignored."
                ]
            er <- startEditorOn taf
            liftIO $
                case er of
                    EditingFailure err ->
                        die $ "Failed to edit; " <> T.unpack err
                    EditingSuccess ->
                        (filter (not . T.null) .
                         filter (not . T.isPrefixOf "#") . T.lines) <$>
                        T.readFile (toFilePath taf)

tmpAliasFile :: MonadIO m => m (Path Abs File)
tmpAliasFile = liftIO $ resolveFile' "/tmp/suggestion-aliases.txt"

getMergedEntry ::
       (MonadIO m, MonadReader Settings m)
    => Suggestion EntrySuggestion
    -> m (Maybe PersonEntry)
getMergedEntry s = do
    let EntrySuggestion {..} = suggestionData s
    oldEntry <-
        case entrySuggestionLikelyRelevantPerson of
            Nothing -> pure Nothing
            Just (uuid, _) -> runData $ getPersonEntry uuid
    pure $ mergeCurrentEntryWithSuggestion oldEntry entrySuggestionEntry

mergeCurrentEntryWithSuggestion ::
       Maybe PersonEntry -> PersonEntry -> Maybe PersonEntry
mergeCurrentEntryWithSuggestion Nothing pe = Just pe
mergeCurrentEntryWithSuggestion (Just pe) sug =
    personEntry $ go (personEntryProperties pe) (personEntryProperties sug)
  where
    go (PVal v1) (PVal v2) = PList [PVal v1, PVal v2]
    go (PVal v1) (PList vs2) = PList $ PVal v1 : vs2
    go (PVal v1) (PMap vs2) = PMap $ ("dummy", PVal v1) : vs2
    go (PList vs1) (PVal v2) = PList $ vs1 ++ [PVal v2]
    go (PList vs1) (PList vs2) = PList $ vs1 ++ vs2
    go (PList vs1) (PMap vs2) = PMap $ ("dummy", PList vs1) : vs2
    go (PMap vs1) (PVal v2) = PMap $ vs1 ++ [("new", PVal v2)]
    go (PMap vs1) (PList vs2) = PMap $ vs1 ++ [("new", PList vs2)]
    go (PMap vs1) (PMap vs2) = PMap $ nubBy ((==) `on` fst) $ vs1 ++ vs2

promptAboutEntry ::
       (MonadIO m, MonadReader Settings m)
    => Maybe PersonEntry
    -> m (Maybe PersonEntry)
promptAboutEntry mEntry =
    case mEntry of
        Nothing -> do
            liftIO $ T.putStrLn "The suggestion is to have no entry."
            pure Nothing
        Just entry -> do
            liftIO $
                case TE.decodeUtf8' $ tmpEntryFileContents entry of
                    Left err ->
                        T.putStrLn $
                        "Failed to decode UTF8 Text in entry contents: " <>
                        T.pack (show err)
                    Right contents -> T.putStrLn contents
            yn <-
                liftIO $
                promptYesNo
                    No
                    "This is the suggested person entry, do you want use it as-is (y) or edit it (n)?"
            case yn of
                Yes -> pure mEntry
                No -> do
                    tef <- tmpEntryFile
                    liftIO $
                        SB.writeFile (toFilePath tef) $
                        tmpEntryFileContents entry
                    er <- startEditorOn tef
                    case er of
                        EditingFailure err ->
                            liftIO $ die $ "Failed to edit; " <> T.unpack err
                        EditingSuccess -> do
                            contents <- liftIO $ SB.readFile $ toFilePath tef
                            case parseEntryFileContents contents of
                                Left err ->
                                    liftIO $
                                    die $
                                    "Unable to parse entry file: " <> show err
                                Right (ForEditor personEntryMap) -> do
                                    now <- liftIO getCurrentTime
                                    case reconstructPersonEntry
                                             now
                                             entry
                                             personEntryMap of
                                        Nothing ->
                                            liftIO $
                                            die $
                                            unwords
                                                [ "Failed to reconstruct a person entry: edit resulted in an invalid person entry"
                                                ]
                                        Just pe -> pure $ Just pe

tmpEntryFile :: MonadIO m => m (Path Abs File)
tmpEntryFile = liftIO $ resolveFile' "/tmp/suggestion-entry.yaml"
