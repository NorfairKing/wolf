{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Entry
    ( entry
    , tmpEntryFileContents
    ) where

import Import

import qualified Data.ByteString as SB
import Data.Time

import Wolf.Cli.Command.Entry.Internal
import Wolf.Cli.Editor
import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data
import Wolf.Data.Git

entry :: (MonadIO m, MonadReader Settings m) => Alias -> m ()
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
                                            , aliasString person
                                            ]
