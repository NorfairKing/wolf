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

instance GenUnchecked Export

instance GenValid Export where
    genValid = do
        eid <- genValid
        -- Any person index
        epi <- genValid
        -- The available person Id's are exactly the ones in the index.
        -- TODO: leave some people out of the index.
        let eps = M.elems $ indexMap epi
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
        ees <- genValid
        eues <- genListOf $ genValid `suchThat` (`notElem` ees)
        pure
            Export
            { exportInitData = eid
            , exportPersonIndex = epi
            , exportPeople = eps
            , exportPersonEntries = epes
            , exportNoteIndex = eni
            , exportNoteIndices = enis
            , exportNotes = ens
            , exportEntrySuggestions = ees
            , exportUsedEntrySuggestions = eues
            }
