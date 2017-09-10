{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.NoteIndex.Types.Gen where

import Import

import Wolf.Data.NoteIndex.Types
import Wolf.Data.People.Types.Gen ()

instance GenUnchecked NoteIndex

instance GenValid NoteIndex

instance GenUnchecked NoteUuid

instance GenValid NoteUuid where
    genValid = NoteUuid <$> genValid
