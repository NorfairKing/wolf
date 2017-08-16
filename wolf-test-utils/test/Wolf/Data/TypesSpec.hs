{-# LANGUAGE TypeApplications #-}

module Wolf.Data.TypesSpec
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
    jsonSpec @NoteIndex
    jsonSpec @NoteUuid
