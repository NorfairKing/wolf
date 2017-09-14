{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Export.Types.Gen where

import Import

import Wolf.Data.Export.Types

import Wolf.Data.Index.Types.Gen ()
import Wolf.Data.Init.Types.Gen ()
import Wolf.Data.Note.Types.Gen ()
import Wolf.Data.NoteIndex.Types.Gen ()
import Wolf.Data.People.Types.Gen ()
import Wolf.Data.Suggestion.Types.Gen ()

instance GenUnchecked Export

instance GenValid Export where
    genValid =
        Export <$> genValid <*> genValid <*> genValid <*> genValid <*> genValid <*>
        genValid <*>
        genValid <*>
        genValid
