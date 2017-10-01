{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.People
    ( PersonUuid
    , personUuidBs
    , personUuidLBs
    , personUuidString
    , personUuidText
    , nextRandomPersonUuid
    , parsePersonUuid
    , parsePersonUuidString
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
    let relDirs = mapMaybe (stripDir pd) dirs
    pure $
        mapMaybe
            (parsePersonUuidString . FP.dropTrailingPathSeparator . toFilePath)
            relDirs
