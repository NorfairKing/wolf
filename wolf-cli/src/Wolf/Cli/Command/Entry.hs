{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Entry where

import Import

import Data.Time

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data.Editor
import Wolf.Data.Entry
import Wolf.Data.Git
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Path
import Wolf.Data.Types

entry :: (MonadIO m, MonadReader Settings m) => String -> m ()
entry person =
    runData $
    withInitCheck $ do
        origIndex <- getIndex
        (personUuid, index) <- lookupOrCreateNewPerson person origIndex
        tmpFile <- tmpPersonEntryFile personUuid
        mPersonEntry <- getPersonEntry personUuid
        (origPersonEntry, inFilePersonEntry) <-
            case mPersonEntry of
                Nothing -> do
                    now <- liftIO getCurrentTime
                    pure $
                        (,) newPersonEntry $
                        case parseFirstnameLastname person of
                            Nothing -> newPersonEntry
                            Just (fn, ln) ->
                                PersonEntry
                                { personEntryProperties =
                                      [ ( "first name"
                                        , PersonPropertyValue fn now)
                                      , ( "last name"
                                        , PersonPropertyValue ln now)
                                      ]
                                }
                Just pe -> pure (pe, pe)
        ensureDir $ parent tmpFile
        let tmpFileContents =
                tmpEntryFileContents person personUuid inFilePersonEntry
        liftIO $ writeFile (toFilePath tmpFile) tmpFileContents
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
                contents <- liftIO $ readFile $ toFilePath tmpFile
                case parseEntryFileContents contents of
                    Left err ->
                        liftIO $
                        die $ unwords ["Unable to parse entry file:", err]
                    Right personEntryMap -> do
                        now <- liftIO getCurrentTime
                        let personEntry =
                                reconstructPersonEntry
                                    now
                                    origPersonEntry
                                    personEntryMap
                        unless (personEntry == origPersonEntry) $ do
                            putPersonEntry personUuid personEntry
                            putIndex index
                            makeGitCommit $
                                unwords ["Added/changed entry for", person]
