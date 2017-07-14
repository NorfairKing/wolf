{-# LANGUAGE TypeApplications #-}

module Wolf.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Types

import Wolf.Types.Gen ()

spec :: Spec
spec = do
    jsonSpecOnValid @InitData
    jsonSpec @Index
    jsonSpec @PersonUuid
    jsonSpecOnValid @PersonEntry
    jsonSpecOnValid @PersonPropertyValue
    jsonSpec @NoteIndex
    jsonSpec @PersonNoteUuid
    jsonSpecOnValid @PersonNote
