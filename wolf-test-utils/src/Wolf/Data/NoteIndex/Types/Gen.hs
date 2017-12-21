{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.NoteIndex.Types.Gen
    ( subNoteIndex
    ) where

import Import

import qualified Data.Set as S

import Wolf.Data.NoteIndex.Types
import Wolf.Data.People.Types.Gen ()

instance GenUnchecked NoteIndex

instance GenValid NoteIndex where
    genValid = NoteIndex <$> genValid

instance GenUnchecked NoteUuid

instance GenValid NoteUuid where
    genValid = NoteUuid <$> genValid

subNoteIndex :: NoteIndex -> Gen NoteIndex
subNoteIndex (NoteIndex nis) =
    (NoteIndex . S.fromList) <$> sublistOf (S.toList nis)
