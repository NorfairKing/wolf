{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Export
    ( Repo
    , repoInitData
    , repoPersonIndex
    , exportRepo
    ) where

import Import

import qualified Data.Map as M

import Wolf.Data.Export.Types
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.People
import Wolf.Data.Suggestion
import Wolf.Data.Types

exportRepo :: (MonadIO m, MonadReader DataSettings m) => m (Maybe Repo)
exportRepo = do
    mid <- readInitData
    case mid of
        Nothing -> pure Nothing
        Just initData -> do
            mi <- getIndexWithDefault
            people <- getPersonUuids
            let mKeyed ::
                       (Ord a, Monad m)
                    => (a -> m (Maybe b))
                    -> [a]
                    -> m (Map a b)
                mKeyed func ls =
                    (M.fromList . mapMaybe (\(p, e) -> (,) p <$> e)) <$>
                    mapM (\p -> (,) p <$> func p) ls
            entries <- mKeyed getPersonEntry people
            noteIndex <- getNoteIndex
            noteIxs <- mKeyed getPersonNoteIndex people
            noteUuids <- getNoteUuids
            notes <- mKeyed readNote noteUuids
            entrySuggestions <- readPersonEntrySuggestions
            usedEntrySuggestions <- readUsedPersonEntrySuggestions
            pure $
                Just
                    Repo
                    { repoInitData = initData
                    , repoPersonIndex = mi
                    , repoPersonEntries = entries
                    , repoNoteIndex = noteIndex
                    , repoNoteIndices = noteIxs
                    , repoNotes = notes
                    , repoEntrySuggestions = entrySuggestions
                    , repoUsedEntrySuggestions = usedEntrySuggestions
                    }
