{-# LANGUAGE TypeApplications #-}

module Wolf.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Types

import Wolf.Data.Types.Gen ()

spec :: Spec
spec = do
    jsonSpecOnValid @InitData
    jsonSpecOnValid @Index
    jsonSpec @PersonUuid
    jsonSpecOnValid @PersonEntry
    jsonSpecOnValid @PersonPropertyValue
    jsonSpec @NoteIndex
    jsonSpec @PersonNoteUuid
    jsonSpecOnValid @PersonNote
