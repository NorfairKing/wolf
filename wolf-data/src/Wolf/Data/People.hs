{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.People
    ( PersonUuid
    , getPersonUuids
    ) where

import Import

import qualified System.FilePath as FP

import Wolf.Data.Path
import Wolf.Data.People.Types
import Wolf.Data.Types

getPersonUuids :: (MonadIO m, MonadReader DataSettings m) => m [PersonUuid]
getPersonUuids = do
    pd <- peopleDir
    dirs <- liftIO $ fmap (fromMaybe []) $ forgivingAbsence $ fst <$> listDir pd
    let relDirs = mapMaybe (stripProperPrefix pd) dirs
    pure $
        mapMaybe
            (parseUUIDString . FP.dropTrailingPathSeparator . toFilePath)
            relDirs
