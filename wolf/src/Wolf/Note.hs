{-# LANGUAGE TemplateHaskell #-}

module Wolf.Note where

import Import

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M

import System.Environment
import System.Process

import Wolf.Types

wolfDir
    :: MonadIO m
    => m (Path Abs Dir)
wolfDir = (</> $(mkRelDir ".wolf")) <$> liftIO getHomeDir

indexFile
    :: MonadIO m
    => m (Path Abs File)
indexFile = do
    wd <- wolfDir
    liftIO $ resolveFile wd "index.json"

readJSON
    :: (MonadIO m, FromJSON a)
    => Path Abs File -> m a
readJSON path = do
    contents <- liftIO $ LB.readFile $ toFilePath path
    case JSON.eitherDecode contents of
        Left decodeErr ->
            liftIO $
            die $
            unwords
                [ "Failed to read JSON file:"
                , toFilePath path
                , "with err"
                , decodeErr
                ]
        Right res -> pure res

readJSONWithDefault
    :: (MonadIO m, FromJSON a)
    => a -> Path Abs File -> m a
readJSONWithDefault def path =
    fmap (fromMaybe def) $ liftIO $ forgivingAbsence $ readJSON path

writeJSON
    :: (MonadIO m, ToJSON a)
    => Path Abs File -> a -> m ()
writeJSON path value =
    liftIO $ do
        ensureDir $ parent path
        LB.writeFile (toFilePath path) (JSON.encodePretty value)

getIndex
    :: MonadIO m
    => m Index
getIndex = indexFile >>= readJSONWithDefault newIndex

putIndex
    :: MonadIO m
    => Index -> m ()
putIndex index = do
    i <- indexFile
    writeJSON i index

lookupInIndex :: String -> Index -> Maybe PersonUuid
lookupInIndex person index = M.lookup person (indexMap index)

lookupOrCreateNewPerson
    :: MonadIO m
    => String -> Index -> m (PersonUuid, Index)
lookupOrCreateNewPerson person origIndex =
    case lookupInIndex person origIndex of
        Nothing -> do
            uuid <- nextRandomPersonUuid
            pure
                ( uuid
                , origIndex
                  {indexMap = M.insert person uuid $ indexMap origIndex})
        Just i -> pure (i, origIndex)

peopleDir
    :: MonadIO m
    => m (Path Abs Dir)
peopleDir = do
    wd <- wolfDir
    liftIO $ resolveDir wd "people"

personDir
    :: MonadIO m
    => PersonUuid -> m (Path Abs Dir)
personDir personUuid = do
    pd <- peopleDir
    liftIO $ resolveDir pd $ personUuidString personUuid

personEntryFile
    :: MonadIO m
    => PersonUuid -> m (Path Abs File)
personEntryFile personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveFile pd "entry.json"

getPersonEntry
    :: MonadIO m
    => PersonUuid -> m PersonEntry
getPersonEntry personUuid =
    personEntryFile personUuid >>= readJSONWithDefault newPersonEntry

noteIndexFile
    :: MonadIO m
    => PersonUuid -> m (Path Abs File)
noteIndexFile personUuid = do
    wd <- personDir personUuid
    liftIO $ resolveFile wd "notes-index.json"

getNoteIndex
    :: MonadIO m
    => PersonUuid -> m NoteIndex
getNoteIndex personUuid =
    noteIndexFile personUuid >>= readJSONWithDefault newNoteIndex

putNoteIndex
    :: MonadIO m
    => PersonUuid -> NoteIndex -> m ()
putNoteIndex personUuid noteIndex = do
    i <- noteIndexFile personUuid
    writeJSON i noteIndex

lookupInNoteIndex :: PersonNoteUuid -> NoteIndex -> Maybe PersonNoteUuid
lookupInNoteIndex noteUuid noteIndex =
    find (== noteUuid) $ noteIndexList noteIndex

createNewNote
    :: MonadIO m
    => PersonUuid -> NoteIndex -> m (PersonNoteUuid, NoteIndex)
createNewNote person noteIndex = do
    noteUuid <- nextRandomPersonNoteUuid
    case lookupInNoteIndex noteUuid noteIndex of
        Nothing ->
            pure
                ( noteUuid
                , noteIndex
                  {noteIndexList = sort $ noteUuid : noteIndexList noteIndex})
        Just _ -> createNewNote person noteIndex -- Just try again

personNotesDir
    :: MonadIO m
    => PersonUuid -> m (Path Abs Dir)
personNotesDir personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveDir pd "notes"

personNoteFile
    :: MonadIO m
    => PersonUuid -> PersonNoteUuid -> m (Path Abs File)
personNoteFile personUuid personNoteUuid = do
    pnd <- personNotesDir personUuid
    liftIO $ resolveFile pnd $ personNoteUuidString personNoteUuid

note
    :: MonadIO m
    => String -> m ()
note person = do
    origIndex <- getIndex
    liftIO $ print origIndex
    (personUuid, index) <- lookupOrCreateNewPerson person origIndex
    liftIO $ print personUuid
    liftIO $ print index
    personEntry <- getPersonEntry personUuid
    liftIO $ print personEntry
    origNoteIndex <- getNoteIndex personUuid
    (noteUuid, noteIndex) <- createNewNote personUuid origNoteIndex
    liftIO $ print noteUuid
    liftIO $ print noteIndex
    editingResult <- startNoteEditor personUuid noteUuid
    case editingResult of
        EditingFailure reason ->
            liftIO $
            putStrLn $
            unwords
                ["ERROR: failed to edit the note file:", reason, ",not saving."]
        EditingSuccess -> do
            putIndex index
            putNoteIndex personUuid noteIndex

startNoteEditor
    :: MonadIO m
    => PersonUuid -> PersonNoteUuid -> m EditingResult
startNoteEditor personUuid noteUuid = do
    liftIO $ print ("Starting note editor for:", personUuid, noteUuid)
    nf <- personNoteFile personUuid noteUuid
    startEditorOn nf

startEditorOn
    :: MonadIO m
    => Path Abs File -> m EditingResult
startEditorOn path = do
    ensureDir $ parent path
    let getContentsOfFile =
            liftIO $ forgivingAbsence $ SB.readFile $ toFilePath path
    contentsBefore <- getContentsOfFile
    meditor <- liftIO $ lookupEnv "EDITOR"
    let editor = fromMaybe "vim" meditor
    let cp = proc editor [toFilePath path]
    liftIO $ print $ unwords [editor, toFilePath path]
    ec <-
        liftIO $ do
            (_, _, _, ph) <- liftIO $ createProcess cp
            liftIO $ waitForProcess ph
    contentsAfter <- getContentsOfFile
    case ec of
        ExitFailure code ->
            pure $
            EditingFailure $
            unwords
                [ "Invoking"
                , editor
                , "on"
                , toFilePath path
                , "failed with exit code"
                , show code
                ]
        ExitSuccess ->
            pure $
            if contentsBefore == contentsAfter
                then EditingFailure $
                     unwords
                         [ "Nothing was changed in file"
                         , toFilePath path
                         , "after editing it with"
                         , editor
                         ]
                else EditingSuccess
