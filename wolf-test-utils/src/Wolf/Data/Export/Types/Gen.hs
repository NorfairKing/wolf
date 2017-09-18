{-# OPTIONS_GHC -fno-warn-orphans #-}

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
        eid <- genValid
        -- Any list of persons
        eps <- genValid
        -- The person index consists of (some) people from the global index
        -- and some occur multiple times.
        epi <-
            let go ix puuid = do
                    b <- genValid
                    if b
                        then pure ix
                        else do
                            a <- genValid
                            go (addIndexEntry a puuid ix) puuid
            in foldM go newIndex eps
        -- For some people, make a person entry.
        epes <-
            fmap catMaybes $
            forM eps $ \p -> do
                b <- genValid
                if b
                    then (Just . (,) p) <$> genValid
                    else pure Nothing
        -- Any global note index.
        eni <- genValid
        let noteUuids = S.toList $ noteIndexSet eni
        -- For some people, make a note index with a sub note index of the global note index.
        enis <-
            fmap catMaybes $
            forM eps $ \p -> do
                b <- genValid
                if b
                    then (Just . (,) p) <$> subNoteIndex eni
                    else pure Nothing
        -- For each noteuuid, make a note.
        ens <- forM noteUuids $ \uuid -> (,) uuid <$> genValid
        -- Two distinct lists of suggestions
        ees <- genValid
        eues <- genListOf $ genValid `suchThat` (`notElem` ees)
        pure
            Repo
            { repoInitData = eid
            , repoPersonIndex = epi
            , repoPeople = eps
            , repoPersonEntries = epes
            , repoNoteIndex = eni
            , repoNoteIndices = enis
            , repoNotes = ens
            , repoEntrySuggestions = ees
            , repoUsedEntrySuggestions = eues
            }
