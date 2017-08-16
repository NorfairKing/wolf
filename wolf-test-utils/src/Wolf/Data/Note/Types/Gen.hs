{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Note.Types.Gen where

import Import

import Wolf.Data.Note.Types

instance GenUnchecked PersonNote

instance GenValid PersonNote
