{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Note.Types.Gen where

import Import

import Wolf.Data.Note.Types

import Wolf.Data.People.Types.Gen ()

instance GenUnchecked Note

instance GenValid Note where
    genValid = Note <$> genValid <*> genValid <*> genValid
