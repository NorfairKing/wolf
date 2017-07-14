{-# LANGUAGE FlexibleContexts #-}

module Wolf.Command.Note where

import Import

import qualified Data.Text.IO as T
import Data.Time

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Editor
import Wolf.Git
import Wolf.Index
import Wolf.Init
import Wolf.JSONUtils
import Wolf.NoteIndex
import Wolf.Path
import Wolf.Types

note :: (MonadIO m, MonadReader Settings m) => String -> m ()
note person =
    runData $
    withInitCheck $ do
        origIndex <- getIndex
        (personUuid, index) <- lookupOrCreateNewPerson person origIndex
        origNoteIndex <- getNoteIndex personUuid
        (noteUuid, noteIndex) <- createNewNote personUuid origNoteIndex
        tnf <- tmpPersonNoteFile personUuid noteUuid
        editingResult <- startEditorOn tnf
        case editingResult of
            EditingFailure reason ->
                liftIO $
                putStrLn $
                unwords
                    [ "ERROR: failed to edit the note file:"
                    , show reason
                    , ",not saving."
                    ]
            EditingSuccess -> do
                now <- liftIO getCurrentTime
                contents <- liftIO $ T.readFile $ toFilePath tnf
                let personNote =
                        PersonNote
                        { personNoteContents = contents
                        , personNoteTimestamp = now
                        }
                nf <- personNoteFile personUuid noteUuid
                writeJSON nf personNote
                putIndex index
                putNoteIndex personUuid noteIndex
                makeGitCommit $
                    unwords
                        [ "Added note on"
                        , person
                        , "with uuid"
                        , personNoteUuidString noteUuid
                        ]
