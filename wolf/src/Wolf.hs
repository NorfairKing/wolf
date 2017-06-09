{-# LANGUAGE TemplateHaskell #-}

module Wolf where

import Import

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M

import Wolf.Types

import Wolf.OptParse

wolf :: IO ()
wolf = do
    (DispatchNote person, Settings) <- getInstructions
    note person

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

lookupOrCreateNew
    :: MonadIO m
    => String -> Index -> m (PersonUuid, Index)
lookupOrCreateNew person origIndex =
    case lookupInIndex person origIndex of
        Nothing -> do
            uuid <- nextRandomPersonUuid
            pure $
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
personNotesDir
    :: MonadIO m
    => PersonUuid -> m (Path Abs Dir)
personNotesDir personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveDir pd "notes"

personNoteFile :: MonadIO m => PersonUuid -> PersonNoteUuid -> m (Path Abs File)
personNoteFile personUuid personNoteUuid = do
    pnd <- personNotesDir personUuid
    liftIO $ resolveFile pnd $ personNoteUuidString personNoteUuid

-- personNotes :: PersonUuid -> m [PersonNote]
-- personNotes personUuid =

note
    :: MonadIO m
    => String -> m ()
note person = do
    origIndex <- getIndex
    liftIO $ print origIndex
    (personUuid, index) <- lookupOrCreateNew person origIndex
    liftIO $ print index
    personEntry <- getPersonEntry personUuid
    liftIO $ print personEntry
