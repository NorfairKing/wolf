{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Export
  ( Repo
  , repoInitData
  , repoPersonIndex
  , exportRepo
  , CautiousExport
  , prettyShowExportWarning
  , prettyShowExportError
  , ExportWarning
  , ExportProblem(..)
  , ExportError(..)
  ) where

import Import

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State.Strict

import Wolf.Data.Cautious
import Wolf.Data.Export.Types
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.NoteIndex.Types
import Wolf.Data.People
import Wolf.Data.Suggestion
import Wolf.Data.Types

import Cautious.CautiousT

getNoteMap ::
     (MonadIO m, MonadReader DataSettings m)
  => [NoteUuid]
  -> m (Map NoteUuid Note)
getNoteMap nus =
  fmap (M.fromList . catMaybes) $
  forM nus $ \nu -> do
    note <- readNote nu
    pure $ (,) nu <$> note

getPeopleMap ::
     (MonadIO m, MonadReader DataSettings m)
  => [PersonUuid]
  -> m (Map PersonUuid NoteIndex)
getPeopleMap pus =
  fmap (M.fromList . catMaybes) $
  forM pus $ \pu -> do
    mNoteIndex <- getPersonNoteIndex pu
    pure $ (,) pu <$> mNoteIndex

checkNotesAndPeople :: Monad m => NAPState m
checkNotesAndPeople = do
  mapM_ checkNote . M.keys . notes =<< get
  mapM_ checkPerson . M.keys . people =<< get

exportRepo :: (MonadIO m, MonadReader DataSettings m) => CautiousExport m Repo
exportRepo = do
  mid <- lift readInitData
  initData <- cautiousErrorIfNothing mid NoInitFile
  mi <- lift getIndexWithDefault
  pus <- lift getPersonUuids -- These are collected from the people directory
  entries <- lift $ mKeyed getPersonEntry pus
  noteIndex <- lift getNoteIndex
  noteUuids <- lift getNoteUuids
  unsafeNoteMap <- lift $ getNoteMap noteUuids
  unsafePersonMap <- lift $ getPeopleMap pus
  NotesAndPeople noteMap mNoteIxs <-
    execStateT checkNotesAndPeople $
    NotesAndPeople unsafeNoteMap unsafePersonMap
  sugs <- lift readAllSuggestions
  realNoteIndex <-
    let ni = NoteIndex $ S.fromList $ M.keys noteMap
     in if ni == noteIndex
          then pure ni
          else cautiousProblem
                 (ExportWarningIncorrectNoteIndex
                    (difference noteIndex ni)
                    (difference ni noteIndex))
                 ni
  let uncheckedRepo =
        Repo
          { repoInitData = initData
          , repoPersonIndex = mi
          , repoPersonEntries = entries
          , repoNoteIndex = realNoteIndex
          , repoNoteIndices = mNoteIxs
          , repoNotes = noteMap
          , repoSuggestions = sugs
          }
  case eitherInvalidRepoMessage uncheckedRepo of
    Left err -> cautiousError $ ExportErrorRepoInvalid err
    Right repo -> pure repo
  where
    mKeyed :: (Ord a, Monad m) => (a -> m (Maybe b)) -> [a] -> m (Map a b)
    mKeyed func ls =
      (M.fromList . mapMaybe (\(p, e) -> (,) p <$> e)) <$>
      mapM (\p -> (,) p <$> func p) ls
    difference (NoteIndex ni) (NoteIndex ni') = NoteIndex $ S.difference ni ni'
