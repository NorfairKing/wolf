{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Entry
    ( entry
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
    withInitCheck_ $ do
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
        liftIO $
            SB.writeFile (toFilePath tmpFile) $
            entryContentsBS inFilePersonEntry
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
                now <- liftIO getCurrentTime
                case updatePersonEntry now origPersonEntry contents of
                    UpdateParseFailure ype ->
                        liftIO $
                        die $
                        "Unable to parse entry file:\n" ++
                        prettyPrintEntryParseException ype
                    UpdateValidityFailure ->
                        liftIO $
                        die
                            "Unable to reconstruct entry after update: Invalid entry."
                    UpdateUnchanged ->
                        liftIO $ putStrLn "Entry was not changed."
                    UpdateSuccess pe -> do
                        putPersonEntry personUuid pe
                        putIndex index
                        makeGitCommit $
                            unwords
                                ["Added/changed entry for", aliasString person]
                    UpdateWasDeletion ->
                        liftIO $ putStrLn "Empty entry will not be saved."
