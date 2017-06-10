module Wolf.Index where

import Import

import qualified Data.Map as M

import Wolf.JSONUtils
import Wolf.Path
import Wolf.Types

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

addIndexEntry :: String -> PersonUuid -> Index -> Index
addIndexEntry person uuid origIndex =
    origIndex {indexMap = M.insert person uuid $ indexMap origIndex}

lookupOrCreateNewPerson
    :: MonadIO m
    => String -> Index -> m (PersonUuid, Index)
lookupOrCreateNewPerson person origIndex =
    case lookupInIndex person origIndex of
        Nothing -> do
            uuid <- nextRandomPersonUuid
            pure (uuid, addIndexEntry person uuid origIndex)
        Just i -> pure (i, origIndex)

getPersonEntry
    :: MonadIO m
    => PersonUuid -> m (Maybe PersonEntry)
getPersonEntry personUuid =
    personEntryFile personUuid >>= readJSONWithDefault Nothing

getPersonEntryOrNew
    :: MonadIO m
    => PersonUuid -> m PersonEntry
getPersonEntryOrNew personUuid =
    personEntryFile personUuid >>= readJSONWithDefault newPersonEntry

putPersonEntry
    :: MonadIO m
    => PersonUuid -> PersonEntry -> m ()
putPersonEntry personUuid personEntry = do
    pef <- personEntryFile personUuid
    writeJSON pef personEntry
