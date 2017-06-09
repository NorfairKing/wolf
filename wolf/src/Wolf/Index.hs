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
