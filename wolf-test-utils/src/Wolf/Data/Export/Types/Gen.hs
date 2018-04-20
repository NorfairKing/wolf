{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types.Gen where

import Import

import qualified Data.Map as M
import qualified Data.Set as S

import Wolf.Data.Export.Types
import Wolf.Data.Index.Types
import Wolf.Data.NoteIndex.Types

import Wolf.Data.Index.Types.Gen ()
import Wolf.Data.Init.Types.Gen ()
import Wolf.Data.Note.Types.Gen ()
import Wolf.Data.NoteIndex.Types.Gen
import Wolf.Data.People.Types.Gen ()
import Wolf.Data.Suggestion.Types.Gen ()

instance GenUnchecked Repo

instance GenValid Repo where
    genValid = do
        repoInitData <- genValid
        -- Any list of persons
        eps <- genValid
        -- The person index consists of (some) people from the global index
        -- and some occur multiple times.
        repoPersonIndex <-
            let go ix puuid = do
                    b <- frequency [(3, pure False), (1, pure True)]
                    if b
                        then pure ix
                        else do
                            a <- genValid
                            go (addIndexEntry a puuid ix) puuid
             in foldM go newIndex eps
        -- For some people, make a person entry.
        repoPersonEntries <-
            fmap (M.fromList . catMaybes) $
            forM eps $ \p -> do
                b <- genValid
                if b
                    then (Just . (,) p) <$> genValid
                    else pure Nothing
        -- Any global note index.
        repoNoteIndex <- genValid
        let noteUuids = S.toList $ noteIndexSet repoNoteIndex
        -- For some people, make a note index with a sub note index of the global note index.
        repoNoteIndices <-
            fmap (M.fromList . catMaybes) $
            forM eps $ \p -> do
                b <- genValid
                if b
                    then (Just . (,) p) <$> subNoteIndex repoNoteIndex
                    else pure Nothing
        -- For each noteuuid, make a note.
        repoNotes <-
            fmap M.fromList $ forM noteUuids $ \uuid -> (,) uuid <$> genValid
        repoSuggestions <- genValid
        pure Repo {..}
