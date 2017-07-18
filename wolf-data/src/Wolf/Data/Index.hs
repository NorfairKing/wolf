{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Index where

import Import

import qualified Data.Map as M

import Wolf.Data.JSONUtils
import Wolf.Data.Path
import Wolf.Data.Types

getIndex :: (MonadIO m, MonadReader DataSettings m) => m Index
getIndex = indexFile >>= readJSONWithDefault newIndex

putIndex :: (MonadIO m, MonadReader DataSettings m) => Index -> m ()
putIndex index = do
    i <- indexFile
    writeJSON i index

lookupInIndex :: Text -> Index -> Maybe PersonUuid
lookupInIndex person index = M.lookup person (indexMap index)

addIndexEntry :: Text -> PersonUuid -> Index -> Index
addIndexEntry person uuid origIndex =
    origIndex {indexMap = M.insert person uuid $ indexMap origIndex}

lookupOrCreateNewPerson ::
       (MonadIO m, MonadReader DataSettings m)
    => Text
    -> Index
    -> m (PersonUuid, Index)
lookupOrCreateNewPerson person origIndex =
    case lookupInIndex person origIndex of
        Nothing -> do
            uuid <- nextRandomPersonUuid
            pure (uuid, addIndexEntry person uuid origIndex)
        Just i -> pure (i, origIndex)

getPersonEntry ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> m (Maybe PersonEntry)
getPersonEntry personUuid =
    personEntryFile personUuid >>= readJSONWithDefault Nothing

putPersonEntry ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> PersonEntry
    -> m ()
putPersonEntry personUuid personEntry = do
    pef <- personEntryFile personUuid
    writeJSON pef personEntry
