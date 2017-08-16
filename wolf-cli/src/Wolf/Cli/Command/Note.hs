{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.Types

note :: (MonadIO m, MonadReader Settings m) => Text -> m ()
note person =
    runData $
    withInitCheck $ do
        origIndex <- getIndexWithDefault
        tnf <- tmpNoteFile
        (personUuid, index) <- lookupOrCreateNewPerson person origIndex
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
                let n =
                        Note
                        { noteContents = contents
                        , noteTimestamp = now
                        , noteRelevantPeople = [personUuid]
                        }
                putIndex index
                noteUuid <- createNewNote n
                makeGitCommit $
                    unwords
                        [ "Added note on"
                        , T.unpack person
                        , "with uuid"
                        , noteUuidString noteUuid
                        ]

tmpNoteFile :: MonadIO m => m (Path Abs File)
tmpNoteFile = do
    tmpDir <- liftIO getTempDir
    liftIO $ resolveFile tmpDir "note.txt"
