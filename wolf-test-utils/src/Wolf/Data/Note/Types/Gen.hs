{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Note.Types.Gen where

import Import

import Wolf.Data.Note.Types

import Wolf.Data.Types.Gen ()

instance GenUnchecked Note

instance GenValid Note
