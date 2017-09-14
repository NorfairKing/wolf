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
        epi <- genValid
        let eps = M.elems $ indexMap epi
        epes <-
            fmap catMaybes $
            forM eps $ \p -> do
                b <- genValid
                if b
                    then (Just . (,) p) <$> genValid
                    else pure Nothing
        eni <- genValid
        let noteUuids = S.toList $ noteIndexSet eni
        enis <-
            fmap catMaybes $
            forM eps $ \p -> do
                b <- genValid
                if b
                    then (Just . (,) p) <$> subNoteIndex eni
                    else pure Nothing
        ens <- forM noteUuids $ \uuid -> (,) uuid <$> genValid
        ees <- genValid
        eues <- genValid
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
