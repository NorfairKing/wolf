{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Note where

import Import

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import Wolf.Cli.Editor
import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data.Git
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.JSONUtils
import Wolf.Data.Note.Types
import Wolf.Data.NoteIndex
import Wolf.Data.Path
import Wolf.Data.Types

note :: (MonadIO m, MonadReader Settings m) => Text -> m ()
note person =
    runData $
    withInitCheck $ do
        origIndex <- getIndexWithDefault
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
                        , T.unpack person
                        , "with uuid"
                        , personNoteUuidString noteUuid
                        ]
