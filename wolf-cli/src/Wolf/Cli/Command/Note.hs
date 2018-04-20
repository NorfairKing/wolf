{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Note where

import Import

import qualified Data.Set as S
import qualified Data.Text.IO as T
import Data.Time

import Wolf.Cli.Editor
import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data
import Wolf.Data.Git

note :: (MonadIO m, MonadReader Settings m) => [Alias] -> m ()
note people =
    runData $
    withInitCheck_ $ do
        origIndex <- getIndexWithDefault
        (peopleUuids, index) <-
            getRelevantPeopleUuidsAndNewIndex people origIndex
        tnf <- tmpNoteFile peopleUuids
        liftIO $ ignoringAbsence $ removeFile tnf
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
                            , noteRelevantPeople = S.fromList peopleUuids
                            }
                putIndex index
                noteUuid <- createNewNote n
                makeGitCommit $
                    unwords
                        [ "Added note on"
                        , intercalate ", " $ map aliasString people
                        , "with uuid"
                        , uuidString noteUuid
                        ]

getRelevantPeopleUuidsAndNewIndex ::
       (MonadIO m, MonadReader DataSettings m)
    => [Alias]
    -> Index
    -> m ([PersonUuid], Index)
getRelevantPeopleUuidsAndNewIndex [] origIndex = pure ([], origIndex)
getRelevantPeopleUuidsAndNewIndex (t:ts) origIndex = do
    (personUuid, index) <- lookupOrCreateNewPerson t origIndex
    (puuids, index') <- getRelevantPeopleUuidsAndNewIndex ts index
    pure (personUuid : puuids, index')

tmpNoteFile :: MonadIO m => [PersonUuid] -> m (Path Abs File)
tmpNoteFile uuids = do
    tmpDir <- liftIO getTempDir
    liftIO $ resolveFile tmpDir $ concatMap uuidString uuids ++ ".txt"
