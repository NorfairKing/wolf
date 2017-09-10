{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Index
    ( indexKeys
    , indexTuples
    , lookupInIndex
    , reverseIndexLookup
    , reverseIndexLookupSingleAlias
    , addIndexEntry
    , createNewPerson
    , addAliases
    , lookupOrCreateNewPerson
    -- * Impure operations
    -- ** Index
    , getIndex
    , getIndexWithDefault
    , putIndex
    -- ** Person Entry
    , getPersonEntry
    , putPersonEntry
    ) where

import Import

import qualified Data.Map as M

import Wolf.Data.Entry.Types
import Wolf.Data.JSONUtils
import Wolf.Data.Path
import Wolf.Data.Types

indexKeys :: Index -> [Text]
indexKeys = M.keys . indexMap

indexTuples :: Index -> [(Text, PersonUuid)]
indexTuples = M.toList . indexMap

-- | Look up a `PersonUuid` in the 'Index' by its alias
lookupInIndex :: Text -> Index -> Maybe PersonUuid
lookupInIndex person index = M.lookup person (indexMap index)

reverseIndexLookup :: PersonUuid -> Index -> [Text]
reverseIndexLookup uuid index =
    map fst $ filter ((== uuid) . snd) (M.toList $ indexMap index)

reverseIndexLookupSingleAlias :: PersonUuid -> Index -> Maybe Text
reverseIndexLookupSingleAlias uuid i =
    case reverseIndexLookup uuid i of
        [] -> Nothing
        (a:_) -> Just a

-- | Add a 'PersonUuid' to the 'Index' at an alias
addIndexEntry :: Text -> PersonUuid -> Index -> Index
addIndexEntry person uuid origIndex =
    origIndex {indexMap = M.insert person uuid $ indexMap origIndex}

-- | Create a new person, if the given aliases was unasigned
createNewPerson ::
       (MonadIO m, MonadReader DataSettings m)
    => [Text]
    -> Index
    -> m (Maybe (PersonUuid, Index))
createNewPerson aliases origIndex = do
    uuid <- nextRandomPersonUuid
    let index = addAliases aliases uuid origIndex
    pure $ (,) uuid <$> index

-- | Add aliases for a 'PersonUuid', if the aliases were all unasigned
addAliases :: [Text] -> PersonUuid -> Index -> Maybe Index
addAliases aliases uuid origIndex =
    if any (isJust . (`lookupInIndex` origIndex)) aliases
        then Nothing
        else let index =
                     foldl
                         (\ix alias -> addIndexEntry alias uuid ix)
                         origIndex
                         aliases
             in Just index

-- | Look up a `PersonUuid` in the 'Index' by its alias
-- if the index does not exist, try looking up the text as a uuid.
-- If neither exist, then create a new 'PersonUuid' and put it in the
-- 'Index' at the given alias if it does not exist yet.
lookupOrCreateNewPerson ::
       (MonadIO m, MonadReader DataSettings m)
    => Text
    -> Index
    -> m (PersonUuid, Index)
lookupOrCreateNewPerson person origIndex =
    case lookupInIndex person origIndex of
        Just i -> pure (i, origIndex)
        Nothing ->
            case find ((== person) . personUuidText) $
                 M.elems (indexMap origIndex) of
                Just puuid -> pure (puuid, origIndex)
                Nothing -> do
                    uuid <- nextRandomPersonUuid
                    pure (uuid, addIndexEntry person uuid origIndex)

-- | Get the index if it is there
getIndex :: (MonadIO m, MonadReader DataSettings m) => m (Maybe Index)
getIndex = indexFile >>= readJSONWithMaybe

-- | Get the index, but return 'Nothing' if it does not exist yet.
getIndexWithDefault :: (MonadIO m, MonadReader DataSettings m) => m Index
getIndexWithDefault = indexFile >>= readJSONWithDefault newIndex

-- | Save an index.
putIndex :: (MonadIO m, MonadReader DataSettings m) => Index -> m ()
putIndex index = do
    i <- indexFile
    writeJSON i index

-- | Get a person's entry by its 'PersonUuid', return Nothing if it does not exist.
getPersonEntry ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> m (Maybe PersonEntry)
getPersonEntry personUuid = personEntryFile personUuid >>= readJSONWithMaybe

-- | Put a person's entry by its 'PersonUuid'.
putPersonEntry ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> PersonEntry
    -> m ()
putPersonEntry personUuid pe = do
    pef <- personEntryFile personUuid
    writeJSON pef pe
