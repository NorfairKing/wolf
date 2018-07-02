{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Cautious
    ( NotesAndPeople(..)
    , ExportNP
    , checkNote
    , checkPerson
    , NAPState
    ) where

import Import

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State.Strict

import Wolf.Data.Export.Types
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.NoteIndex.Types
import Wolf.Data.People

type NAPState m = StateT NotesAndPeople (CautiousExport m) ()

data NotesAndPeople = NotesAndPeople
    { notes :: Map NoteUuid Note
    , people :: Map PersonUuid NoteIndex
    } deriving (Show, Eq)

type ExportNP m = CautiousExport m (Map NoteUuid Note, Map PersonUuid NoteIndex)

checkNote :: Monad m => NoteUuid -> NAPState m
checkNote nu = do
    NotesAndPeople {..} <- get
    case M.lookup nu notes of
        Nothing -> pure ()
        Just note ->
            forM_ (noteRelevantPeople note) $ \pu -> checkPersonFromNote nu pu

checkPerson :: Monad m => PersonUuid -> NAPState m
checkPerson pu = do
    NotesAndPeople {..} <- get
    case M.lookup pu people of
        Nothing -> pure ()
        Just ni -> forM_ (noteIndexSet ni) $ \nu -> checkNoteFromPerson pu nu

checkNoteFromPerson :: Monad m => PersonUuid -> NoteUuid -> NAPState m
checkNoteFromPerson pu nu = do
    NotesAndPeople {..} <- get
    case M.lookup nu notes of
        Nothing -> rmNuFromPerson nu pu
        Just note ->
            if S.member pu $ noteRelevantPeople note
                then pure ()
                else addPersonUuidRef pu nu

checkPersonFromNote :: Monad m => NoteUuid -> PersonUuid -> NAPState m
checkPersonFromNote nu pu = do
    NotesAndPeople {..} <- get
    case M.lookup pu people of
        Nothing -> rmPuFromNote pu nu
        Just ni ->
            if S.member nu $ noteIndexSet ni
                then pure ()
                else addNoteUuidRef nu pu

rmPuFromNote :: Monad m => PersonUuid -> NoteUuid -> NAPState m
rmPuFromNote pu nu = do
    lift $ cautiousProblem (ExportWarningMissingNote nu) ()
    state $ \x@NotesAndPeople {..} ->
        ((), x {notes = M.update (rmPuFromNoteRp pu) nu notes})
  where
    rmPuFromNoteRp pu' note =
        Just $
        note {noteRelevantPeople = S.delete pu' $ noteRelevantPeople note}

rmNuFromPerson :: Monad m => NoteUuid -> PersonUuid -> NAPState m
rmNuFromPerson nu pu = do
    lift $ cautiousProblem (ExportWarningMissingRelevantNote nu pu) ()
    state $ \x@NotesAndPeople {..} ->
        ((), x {people = M.update (rmNuFromNi nu) pu people})
  where
    rmNuFromNi nu' (NoteIndex nis) = Just . NoteIndex $ S.delete nu' nis

addPersonUuidRef :: Monad m => PersonUuid -> NoteUuid -> NAPState m
addPersonUuidRef pu nu = do
    lift $ cautiousProblem (ExportWarningMissingRelevantPerson pu nu) ()
    state $ \x@NotesAndPeople {..} ->
        ((), x {notes = M.update (addPersonUuidToNote pu) nu notes})
  where
    addPersonUuidToNote pu' note =
        Just $
        note {noteRelevantPeople = S.insert pu' $ noteRelevantPeople note}

addNoteUuidRef :: (Monad m) => NoteUuid -> PersonUuid -> NAPState m
addNoteUuidRef nu pu = do
    lift $ cautiousProblem (ExportWarningMissingRelevantNote nu pu) ()
    state $ \x@NotesAndPeople {..} ->
        ( ()
        , x
          { people =
                M.update
                    (Just . NoteIndex . S.insert nu . noteIndexSet)
                    pu
                    people
          })
