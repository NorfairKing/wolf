{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.People
    ( getPersonUuids
    ) where

import Import

import qualified System.FilePath as FP

import Wolf.Data.Path
import Wolf.Data.Types

getPersonUuids :: (MonadIO m, MonadReader DataSettings m) => m [PersonUuid]
getPersonUuids = do
    pd <- peopleDir
    dirs <- liftIO $ fst <$> listDir pd
    let relDirs = mapMaybe (stripDir pd) dirs
    pure $
        mapMaybe
            (parsePersonUuidString . FP.dropTrailingPathSeparator . toFilePath)
            relDirs
