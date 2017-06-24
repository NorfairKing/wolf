{-# LANGUAGE FlexibleContexts #-}

module Wolf.Command.Entry where

import Import

import Data.Time

import Wolf.Editor
import Wolf.Entry
import Wolf.Git
import Wolf.Index
import Wolf.Init
import Wolf.OptParse.Types
import Wolf.Path
import Wolf.Types

entry :: (MonadIO m, MonadReader Settings m) => String -> m ()
entry person =
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
