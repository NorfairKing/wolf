{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Types.Gen where

import TestImport

import Data.UUID.Types

import Wolf.Types

instance GenUnchecked UUID where
    genUnchecked =
        let genWord = fromIntegral <$> (genUnchecked :: Gen Int)
        in fromWords <$> genWord <*> genWord <*> genWord <*> genWord

instance GenUnchecked InitData

instance GenValid InitData

instance GenUnchecked Index

instance GenUnchecked PersonUuid

instance GenUnchecked PersonEntry

instance GenValid PersonEntry

instance GenUnchecked PersonPropertyValue

instance GenValid PersonPropertyValue

instance GenUnchecked NoteIndex

instance GenUnchecked PersonNoteUuid

instance GenUnchecked PersonNote

instance GenValid PersonNote
